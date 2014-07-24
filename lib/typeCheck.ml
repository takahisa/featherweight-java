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
open Ast
open Printf
module L = List
module M = Map.Make (
  struct
    type t = Id.t
    let compare = compare
  end
)
module S = Set.Make (
  struct
    type t = int
    let compare = compare
  end
)

exception Type_error of string

let non_existence_type = Type.make "?"

let base_klass_name = "Object"
let base_klass_type = Type.make base_klass_name
let base_klass = Class.make
  ~name: base_klass_name
  ~super: non_existence_type
  ~fields: []
  ~ctors: [(Ctor.make ~name: "Object" ~params: [] ~body: [] ~super_init: [])]
  ~methods: []

let make_classtable ks =
  let ct = M.singleton base_klass_name base_klass in
  L.fold_left begin fun ct k ->
    let x = Class.name k in
    if M.mem x ct then
      raise (Type_error (sprintf "the class `%s` has already been defined." x));
    M.add x k ct
  end ct ks

let class_of ct x =
  if not (M.mem x ct) then
    raise (Type_error (sprintf "%s is not found in classtable." x));
  M.find x ct

let field_of k x =
  try
    L.find (fun f -> Field.name f = x) (Class.fields k)
  with
    Not_found ->
      raise (Type_error (sprintf "the field `%s` is not member of the class `%s`." x (Class.name k)))

let super_of ct k =
  class_of ct (Type.name (Class.super k))

let is_subclass ct t0 t1 =
(*  check_infinite_type ct t0; *)
(*  check_infinite_type ct t1; *)
  let rec is_subclass ct t0 t1 =
    if t0 = t1 then
      true
    else if t1 = base_klass_type then
      false
    else
      is_subclass ct t0 (Class.super (class_of ct (Type.name t1)))
  in 
    is_subclass ct t0 t1

let distance ct t0 t1 =
(* check_infinite_type ct t0;" *)
(* check_infinite_type ct t1;" *)
(* if not (is_subclass ct t0 t1) then
     raise (Type_error (sprintf "the type `%s` is not subtype of the type `%s`." (Type.name t1) (Type.name t0))); *)
  let rec distance ct t0 t1 d =
    if t0 = t1 then
      d
    else if t1 = base_klass_type then
      raise (Type_error (sprintf "the type `%s` is not subtype of the type `%s`." (Type.name t1) (Type.name t0)))
    else
      distance ct t0 (Class.super (class_of ct (Type.name t1))) (d + 1)
  in
    distance ct t0 t1 0

let check_ambiguous ds =
  let rec check_ambiguous set = function
  | []                       -> ()
  | d :: ds when S.mem d set -> raise (Type_error "ambiguous reference error.");
  | d :: ds                  -> check_ambiguous (S.add d set) ds
  in
  check_ambiguous S.empty ds

let is_appliable_method ct m ts =
  let ts' = L.map snd (Method.params m) in
  L.length ts = L.length ts' && L.for_all2 (is_subclass ct) ts' ts'

let rec findall_appliable_methods ct k x ts =
  let rec findall_appliable_methods ct k x ts ms =
    let ms' = L.filter (fun m -> is_appliable_method ct m ts) (Class.methods k) in
    if Class.typ k = base_klass_type then
      ms' @ ms
    else
      findall_appliable_methods ct k x ts (ms' @ ms)
  in
    findall_appliable_methods ct k x ts [] 

let choice_most_specific_method ct k x ts =
  let appliable_methods = findall_appliable_methods ct k x ts in
  if L.length appliable_methods = 0 then
    raise (Type_error (sprintf "the method `%s` cannot be applied to given types." x));
  let distance_list = L.map begin fun m ->
    let ts' = L.map snd (Method.params m) in
    let distance = L.fold_left (+) 0 (L.map2 (distance ct) ts' ts) in
    (m, distance)
  end appliable_methods in
  check_ambiguous (L.map snd distance_list);
  fst (L.hd (L.sort (fun (_, d0) (_, d1) -> compare d0 d1) distance_list))
  
let choice_most_specific_ctor ct k ts =
  let appliable_ctors = L.filter begin fun c ->
    let ts' = L.map snd (Ctor.params c) in
    L.length ts = L.length ts' && L.for_all2 (is_subclass ct) ts' ts
  end (Class.ctors k) in
  if L.length appliable_ctors = 0 then
    raise (Type_error "ctors cannot be applied to given types.");
  let distance_list = L.map begin fun c ->
    let ts' = L.map snd (Ctor.params c) in
    let distance = L.fold_left (+) 0 (L.map2 (distance ct) ts' ts) in
    (c, distance)
  end appliable_ctors in
  check_ambiguous (L.map snd distance_list);
  fst (L.hd (L.sort (fun (_, d0) (_, d1) -> compare d0 d1) distance_list))

let rec infer_expr ct env = function
| Expr.Var(x0) when M.mem x0 env -> M.find x0 env
| Expr.Var(x0)                   -> raise (Type_error (sprintf "`%s` is not defined in current environment" x0))
| Expr.FieldGet(e0, x0) ->
  let k0 = class_of ct (Type.name (infer_expr ct env e0)) in
  let f0 = field_of k0 x0 in
  Field.typ f0
| Expr.FieldSet(e0, x0, e1) ->
  let k0 = class_of ct (Type.name (infer_expr ct env e0)) in
  let f0 = field_of k0 x0 in
  let t0 = Field.typ f0 in
  let t1 = infer_expr ct env e1 in
  if not (is_subclass ct t0 t1) then
    raise (Type_error (sprintf "`%s` is not subclass of `%s`." (Type.name t1) (Type.name t0)));
  t0
| Expr.MethodCall(e0, x0, es0) ->
  let ts0 = L.map (infer_expr ct env) es0 in
  let k0 = class_of ct (Type.name (infer_expr ct env e0)) in
  let m0 = choice_most_specific_method ct k0 x0 ts0 in
  Method.ret_type m0
| Expr.New(x0, es0) ->
  let ts0 = L.map (infer_expr ct env) es0 in
  let k0 = class_of ct x0 in
  ignore (choice_most_specific_ctor ct k0 ts0);
  Type.name x0
| Expr.Cast(t0, e0) ->
  let t1 = infer_expr ct env e0 in
  if not (is_subclass ct t0 t1) then
    raise (Type_error (sprintf "`%s` is not subclass of `%s`." (Type.name t1) (Type.name t0)));
  t0

let check_infinite_type ct t =
  let rec check_infinite_type ct t0 t1 =
    if t0 = t1 then
      raise (Type_error (sprintf "%s is infinite type" (Type.name t)))
    else if t0 = base_klass_type || t1 = base_klass_type then
      ()
    else
      check_infinite_type ct t0 (Class.super (class_of ct (Type.name t1)))
  in
    check_infinite_type ct t (Class.super (class_of ct (Type.name t)))

let rec check_ctor ct env k c =
  if Class.name k <> Ctor.name c then
    raise (Type_error "invalid constructor name.");
  let env' = L.fold_left (fun env (x0, t0) -> M.add x0 t0 env) env (Ctor.params c) in
  L.iter (fun e -> ignore (infer_expr ct env' e)) (Ctor.body c);
  if not (Class.typ k = base_klass_type) then
    begin let ts = L.map (infer_expr ct env) (Ctor.super_init c) in
          let k' = super_of ct k in
          let c' = choice_most_specific_ctor ct k' ts in
          check_ctor ct env k' c' end;
  ()

let check_field ct env k f =
 let x = Field.name f in
  if M.mem x env then
    raise (Type_error (sprintf "the field %s has already been defined." x));
  ()

let check_method ct env k m =
  let env' = L.fold_left (fun env (x0, t0) -> M.add x0 t0 env) env (Method.params m) in
  ignore (infer_expr ct env' (Method.body m))

let check_class ct env k =
  check_infinite_type ct (Class.typ k);
  let env' = M.add "this" (Class.typ k) env in
  let env' = L.fold_left begin fun env f -> 
    let x = Field.name f in
    let t = Field.typ f in
    check_field ct env k f;
    M.add x t env
  end env' (Class.fields k) in
  L.iter (check_ctor ct env' k) (Class.ctors k);
  L.iter (check_method ct env' k) (Class.methods k)

let check_classtable ct =
  let env = M.empty in
  M.iter (fun _ k -> check_class ct env k) ct

let f klasses =
  let ct = make_classtable klasses in
  check_classtable ct
