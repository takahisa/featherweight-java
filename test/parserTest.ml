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
open Ast

let target =
  "class Pair extends Object\n"
^ "{\n"
^ "  Object fst;\n"
^ "  Object snd;\n"
^ "  Pair(Object fst, Object snd) {\n"
^ "    super();\n"
^ "    this.fst = fst;\n"
^ "    this.snd = snd;\n"
^ "  }\n"
^ "  Pair swap() {\n"
^"     return new Pair(this.snd, this.fst);\n"
^"   }\n"
^ "}\n"

let expected =
  Class.make 
    ~name: "Pair"
    ~super: "Object"
    ~ctors: [(Ctor.make 
                ~name: "Pair"
                ~params: [("fst", "Object"); ("snd", "Object")]
                ~body: [Expr.FieldSet(Expr.Var("this"), "fst", Expr.Var("fst"));
                        Expr.FieldSet(Expr.Var("this"), "snd", Expr.Var("snd"))]
                ~super_init: [])]
    ~fields: [(Field.make ~name: "fst" ~typ: "Object");
              (Field.make ~name: "snd" ~typ: "Object")]
    ~methods: [(Method.make
                  ~name: "swap"
                  ~params: []
                  ~body: (Expr.New("Pair", [(Expr.FieldGet(Expr.Var("this"), "snd")); (Expr.FieldGet(Expr.Var("this"), "fst"))]))
                  ~ret_type: "Pair")]

let parse s =
  Parser.prog Lexer.token (Lexing.from_string s)

let parse_test =
  "parse_test" >:: begin
    fun () ->
      assert_equal [expected] (parse target) 
  end

let _ =
  run_test_tt_main begin
    "Parser" >::: [
      parse_test
    ]
  end
