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

    let anomaly_label = "Quality.repr"
    let anomaly_err q = Pp.(str "Quality " ++ Quality.raw_pr q ++ str " undefined.")
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

let add_quality g q =
  let g = try G.add q g with G.AlreadyDeclared -> g in (* Should it fail? *)
  enforce_constraint (qtype, QConstraint.Leq, q) g

let enforce_eliminates_to g s1 s2 =
  let g = add_quality g s1 in
  let g = add_quality g s2 in
  enforce_constraint (s2, QConstraint.Leq, s1) g

let enforce_eq g s1 s2 =
  let g = add_quality g s1 in
  let g = add_quality g s2 in
  enforce_constraint (s2, QConstraint.Equal, s1) g

(* let add_qvar q g = add_quality g (QVar q) *)

let initial_quality_constraints =
  let open QConstraint in
  let g = G.empty in
  let g = List.fold_left (fun g q -> G.add q g) g Sorts.Quality.all_constants in
  (* Enforces the constant constraints defined in the table of
     [Constants.eliminates_to] without reflexivity. *)
  List.fold_left
    (fun g q ->
      List.fold_left
	(fun g q' -> if eliminates_to q q'
		  then enforce_constraint (q, Lt, q') g
		  else g) g
	(List.filter (fun q' -> not @@ Sorts.Quality.equal q q')
		Sorts.Quality.all_constants))
    g Sorts.Quality.all_constants

(* TTT: Rename to eliminates_to to keep it consistent with Sorts.Quality? *)
let is_allowed_elimination = G.check_leq

let sort_eliminates_to g s1 s2 =
  is_allowed_elimination g (Sorts.quality s1) (Sorts.quality s2)

let check_eq = G.check_eq

let check_eq_sort g s s' = check_eq g (Sorts.quality s) (Sorts.quality s')

let eliminates_to_prop g q = is_allowed_elimination g q Sorts.Quality.qprop

let domain = G.domain

let qvar_domain g =
  let domain = domain g in
  Quality.Set.fold (fun q acc -> match q with QVar q -> QVar.Set.add q acc | _ -> acc) domain QVar.Set.empty

(* could be part of acyclic graph api? *)
let is_empty g = Set.is_empty (domain g)

let add_template_qvars =
  QVar.Set.fold
    (fun v g -> enforce_eliminates_to g (QVar v) Quality.qprop)
