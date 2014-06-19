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
open OUnit
open Lexer
open Parser

let token s =
  Lexer.token (Lexing.from_string s)

let eof_test =
  "eof" >:: begin
    fun () -> assert_equal T_eof (token "")
  end

let whitespace_test =
  "whitespace" >:: begin
    fun () -> assert_equal T_eof (token " \r\n\t")
  end

let lparen_test =
  "lparen" >:: begin
    fun () -> assert_equal T_lparen (token "(")
  end

let rparen_test =
 "rparen" >:: begin
    fun () -> assert_equal T_rparen (token ")")
  end

let lbrace_test =
  "lbrace" >:: begin
    fun () -> assert_equal T_lbrace (token "{")
  end

let rbrace_test =
  "rbrace" >:: begin
    fun () -> assert_equal T_rbrace (token "}")
  end

let period_test =
  "period" >:: begin
    fun () -> assert_equal T_period (token ".")
  end

let comma_test =
  "comma" >:: begin
    fun () -> assert_equal T_comma (token ",")
  end

let semicolon_test =
  "semicolon" >:: begin
    fun () -> assert_equal T_semicolon (token ";")
  end

let assign_test =
  "assign" >:: begin
    fun () -> assert_equal T_assign (token "=")
  end

let ident_test =
  "ident" >:: begin
    fun () -> assert_equal (T_ident "id") (token "id");
      assert_equal (T_ident "Id") (token "Id");
      assert_equal (T_ident "_id") (token "_id");
      assert_equal (T_ident "_id1234") (token "_id1234")
  end

let keyword_test =
  "keyword_test" >:: begin
    fun () -> Hashtbl.iter (fun k v -> assert_equal v (token k)) Lexer.keyword_table
  end

let _ = 
  run_test_tt_main begin
    "Lexer" >::: [
      whitespace_test
    ; eof_test
    ; lparen_test
    ; rparen_test
    ; lbrace_test
    ; rbrace_test
    ; semicolon_test
    ; period_test
    ; comma_test
    ; assign_test
    ; ident_test
    ; keyword_test
    ]
  end

