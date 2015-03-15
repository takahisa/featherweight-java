%{
(*
 * Copyright (c) 2014 - 2015 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)
 open Ast

 type 'a revlist = Snoc of 'a revlist * 'a | Nil

 let rec to_list = function
 | Snoc(xs, x) -> x :: (to_list xs)
 | Nil         -> []

 let to_list_with_rev xs =
   List.rev (to_list xs)
 
 let rec of_list = function
 | x :: xs -> Snoc(of_list xs, x)
 | []      -> Nil

 let rec of_list_with_rev xs =
   of_list (List.rev xs)

%}
%token T_eof
%token T_lparen
%token T_rparen
%token T_lbrace
%token T_rbrace
%token T_semicolon
%token T_period
%token T_comma
%token T_assign
%token <string> T_ident
%token T_keyword_class
%token T_keyword_extends
%token T_keyword_new
%token T_keyword_this
%token T_keyword_super
%token T_keyword_return
%right T_comma
%type <Ast.Class.t list> prog 
%start                   prog
%%

prog:
  | class_definition_list T_eof
    { to_list_with_rev $1 }

class_definition_list:
  | class_definition_list class_definition
    { Snoc($1,$2) }
  |
    { Nil }

class_definition:
  | T_keyword_class T_ident T_keyword_extends T_ident
    T_lbrace
      member_definition_list
    T_rbrace
    { let rec go fs cs ms = function
      | Nil           -> Class.make $2 $4 fs cs ms
      | Snoc(rest, d) -> begin
        match d with
        | `Field  f -> go (f::fs) cs ms rest
        | `Ctor   c -> go fs (c::cs) ms rest
        | `Method m -> go fs cs (m::ms) rest
      end in go [] [] [] $6 }

member_definition_list:
  | member_definition_list member_definition
    { Snoc($1, $2) }
  |
    { Nil }

member_definition:
  | field_definition
    { `Field $1 }
  | ctor_definition
    { `Ctor $1 }
  | method_definition
    { `Method $1 }

field_definition:
  | T_ident T_ident T_semicolon
    { Field.make $2 $1 }

ctor_definition:
  | T_ident T_lparen parameter_list T_rparen
    T_lbrace
      T_keyword_super T_lparen argument_list T_rparen T_semicolon
      field_initializer_list
    T_rbrace
    { Ctor.make $1 $3 (to_list_with_rev $11) $8 }

field_initializer_list:
  | field_initializer_list field_initializer
    { Snoc($1, $2) }
  |
    { Nil }

field_initializer:
  | T_keyword_this T_period T_ident T_assign expression T_semicolon
    { Expr.FieldSet(Expr.Var("this"), $3, $5) }

method_definition:
  | T_ident T_ident T_lparen parameter_list T_rparen
    T_lbrace
      T_keyword_return expression T_semicolon
    T_rbrace
    { Method.make $2 $4 $8 $1 }

parameter_list:
  | parameter_list_rest
    { $1 }
  |
    { [] }

parameter_list_rest:
  | parameter_list_rest T_comma parameter_list_rest
    { List.append $1 $3 }
  | parameter
    { [$1] }

parameter:
  | T_ident T_ident
    { ($2 , $1) }

argument_list:
  | argument_list_rest
    { $1 }
  |
    { [] }

argument_list_rest:
  | argument_list_rest T_comma argument_list_rest
    { List.append $1 $3 }
  | argument
    { [$1] }

argument:
  | expression
    { $1 }

expression:
  | var
    { $1 }
  | field_get
    { $1 }
  | field_set
    { $1 }
  | method_call
    { $1 }
  | new_instance
    { $1 }
  | cast
    { $1 }

var:
  | T_ident
    { Expr.Var $1 }
  | T_keyword_this
    { Expr.Var "this" }

field_get:
  | expression T_period T_ident
    { Expr.FieldGet($1, $3) }

field_set:
  | expression T_period T_ident T_assign expression
    { Expr.FieldSet($1, $3, $5) }

method_call:
  | expression T_period T_ident T_lparen argument_list T_rparen
    { Expr.MethodCall($1, $3, $5) }

new_instance:
  | T_keyword_new T_ident T_lparen argument_list T_rparen
    { Expr.New($2, $4) }

cast:
  | T_lparen T_ident T_rparen expression
    { Expr.Cast($2, $4) }
