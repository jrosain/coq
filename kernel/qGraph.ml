(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)
open Sorts 
open Quality

module G = AcyclicGraph.Make(struct
    type t = Quality.t
    module Set = Quality.Set
    module Map = Quality.Map

    let equal = Quality.equal
    let compare = Quality.compare

    let raw_pr = Quality.raw_pr
  end)

type t = G.t

exception QualitityInconsistency of string

let enforce_constraint0 (q1, cst, q2) g =
  match cst with
  | QConstraint.Leq -> G.enforce_leq q1 q2 g
  | QConstraint.Lt -> G.enforce_lt q1 q2 g
  | QConstraint.Equal -> G.enforce_eq q1 q2 g

let enforce_constraint cstr g =
  match enforce_constraint0 cstr g with
  | None -> raise (QualitityInconsistency "Inconsistency")
  | Some g -> g


(* let add_quality_constraint g q := 
  let g = G.add g q in 
  enforce_constraint (qtype, QConstraint.Lt, q) g *)

  
(* let initial_qualities = [qsprop ; qprop ; qtype]

let base_elimination_constraints q = 
  let open QConstraint in
  match q with
  | QConstant QType -> [(Lt, qprop) ; (Lt, qsprop)]
  | QConstant QProp -> [(Lt, qsprop)]
  | QConstant QSProp -> []
  | QVar _ -> [] *)

let initial_quality_constraints =
  let open QConstraint in
  (* Can generalize to : 
    foreach quality in constants (initial qualities), add to graph + use constant-elimination table to enforce constraints *)
  (* let add_quality_constraints g q = 
    let g = G.add q g in
    List.fold_left (fun g (cst, q') -> enforce_constraint (q, cst, q') g) g (base_elimination_constraints q) in
  List.fold_left add_quality_constraints G.empty initial_qualities *)
  let g = G.empty in
  let g = G.add qtype g in
  let g = G.add qprop g in 
  let g = G.add qsprop g in 
  let g = enforce_constraint (qtype, Lt, qprop) g in
  let g = enforce_constraint (qtype, Lt, qsprop) g in
  let g = enforce_constraint (qprop, Lt, qsprop) g in
  g


let is_allowed_elimination g q1 q2 = 
  (* FIXME : These qualities should already exist in the graph *)
  let g = try G.add q1 g with G.AlreadyDeclared -> g in
  let g = try G.add q2 g with G.AlreadyDeclared -> g in
  G.check_leq g q1 q2