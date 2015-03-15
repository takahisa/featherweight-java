{
(*
 * Copyright (c) 2014 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
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
open Parser
module P = Printf
module L = Lexing

(*
type token = T_eof | T_lparen | T_rparen | T_lbrace | T_rbrace
| T_semicolon | T_period | T_comma | T_assign | T_ident of string
| T_keyword_class
| T_keyword_extends
| T_keyword_this
| T_keyword_new
| T_keyword_super
| T_keyword_return
*)

let keyword_table : (string, token) Hashtbl.t = Hashtbl.create 32
let _ = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
  [("class"   , T_keyword_class)
  ;("extends" , T_keyword_extends)
  ;("this"    , T_keyword_this)
  ;("new"     , T_keyword_new)
  ;("super"   , T_keyword_super)
  ;("return"  , T_keyword_return)]

let error msg lexbuf =
  failwith (P.sprintf "error: %s %d-%d\n" msg
	         (L.lexeme_start lexbuf)
	         (L.lexeme_end lexbuf))

let warning msg lexbuf =
  P.eprintf "warning: %s %d-%d\n" msg
	         (L.lexeme_start lexbuf)
	         (L.lexeme_end lexbuf)
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

let letter = lower | upper

let newline = ['\n' '\r']
let whitespace = [' ' '\n' '\r' '\t']

let op_symbol = ['+' '-' '*' '/' '%' '=' '!' '?' '<' '>' '|' '&' '^' '@']
let op = op_symbol+

rule token = parse
| whitespace+       { token lexbuf }
| "/*"              { comment lexbuf; token lexbuf }
| eof               { T_eof }
| '('               { T_lparen }
| ')'               { T_rparen }
| '{'               { T_lbrace }
| '}'               { T_rbrace }
| ','               { T_comma }
| '.'               { T_period }
| ';'               { T_semicolon }
| '='               { T_assign }
| (letter | '_') (letter | digit | '_')*
  { let ident = L.lexeme lexbuf in
    try
      Hashtbl.find keyword_table ident
    with
      Not_found -> T_ident ident }
|  _
  { error (P.sprintf "unknown token %s near characters" (L.lexeme lexbuf)) lexbuf }

and comment = parse
| eof               { warning "unterminated comment" lexbuf }
| "/*"              { comment lexbuf }
| "*/"              { () }
| _                 { comment lexbuf }
