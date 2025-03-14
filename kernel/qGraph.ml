(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)
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
  let open ElimConstraint in
  match cst with
  | ElimTo -> G.enforce_leq q1 q2 g
  | SElimTo -> G.enforce_lt q1 q2 g
  | Eq -> G.enforce_eq q1 q2 g

let enforce_constraint cstr g =
  match enforce_constraint0 cstr g with
  | None -> raise (QualitityInconsistency "Inconsistency")
  | Some g -> g

let merge_constraints csts g = ElimConstraints.fold enforce_constraint csts g

let check_constraint g (q1, c, q2) =
  let open ElimConstraint in
  match c with
  | ElimTo -> G.check_leq g q1 q2
  | SElimTo -> G.check_lt g q1 q2
  | Eq -> G.check_eq g q1 q2

let check_constraints csts g = ElimConstraints.for_all (check_constraint g) csts

exception AlreadyDeclared = G.AlreadyDeclared
let add_quality g q =
  let g = G.add q g in (* Should it fail? Yes *)
  enforce_constraint (qtype, ElimConstraint.ElimTo, q) g

let enforce_eliminates_to s1 s2 g =
  enforce_constraint (s1, ElimConstraint.ElimTo, s2) g

let enforce_eq g s1 s2 =
  enforce_constraint (s1, ElimConstraint.Eq, s2) g

(* let add_qvar q g = add_quality g (QVar q) *)

let initial_quality_constraints =
  let open Quality in
  let g = G.empty in
  let g = List.fold_left (fun g q -> G.add q g) g all_constants in
  (* Enforces the constant constraints defined in the table of
     [Constants.eliminates_to] without reflexivity. *)
  List.fold_left
    (fun g q ->
      List.fold_left
	(fun g q' -> if eliminates_to q q'
		  then enforce_constraint (q, ElimConstraint.SElimTo, q') g
		  else g) g
	(List.filter (fun q' -> not @@ Quality.equal q q') all_constants))
    g all_constants

(* TTT: Rename to eliminates_to to keep it consistent with Quality? *)
let is_allowed_elimination = G.check_leq

let sort_eliminates_to g s1 s2 =
  is_allowed_elimination g (Sorts.quality s1) (Sorts.quality s2)

let check_eq = G.check_eq

let check_eq_sort g s s' = check_eq g (Sorts.quality s) (Sorts.quality s')

let eliminates_to_prop g q = is_allowed_elimination g q qprop

let domain = G.domain

let qvar_domain g =
  let domain = domain g in
  Quality.Set.fold
    (fun q acc -> match q with QVar q -> QVar.Set.add q acc | _ -> acc)
    domain QVar.Set.empty

(* could be part of acyclic graph api? *)
let is_empty g = Set.is_empty (domain g)

let add_template_qvars =
  QVar.Set.fold
    (fun v -> enforce_eliminates_to (QVar v) Quality.qprop)
