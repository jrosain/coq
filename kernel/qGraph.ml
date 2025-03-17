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

type quality_inconsistency =
  ((Quality.QVar.t -> Pp.t) option) * (ElimConstraint.kind * quality * quality * Pp.t option)

exception QualityInconsistency of quality_inconsistency

(* If s can eliminate to s', we want an edge between s and s'.
   In the acyclic graph, it means setting s to be lower or equal than s'.
   This function ensures a uniform behaviour between [check] and [enforce]. *)
let to_graph_cstr k =
  let open ElimConstraint in
  match k with
    | ElimTo -> AcyclicGraph.Le
    | SElimTo -> AcyclicGraph.Lt
    | Eq -> AcyclicGraph.Eq

let check_func k =
  let open AcyclicGraph in
  match to_graph_cstr k with
  | Le -> G.check_leq
  | Lt -> G.check_lt
  | Eq -> G.check_eq

let enforce_func k =
  let open AcyclicGraph in
  match to_graph_cstr k with
  | Le -> G.enforce_leq
  | Lt -> G.enforce_lt
  | Eq -> G.enforce_eq

let enforce_constraint (q1,k,q2) g =
  match enforce_func k q1 q2 g with
  | None ->
     let m = Pp.str"Impossible to enforce this constraint in the graph." in
     raise @@ QualityInconsistency (None, (k, q1, q2, Some m))
  | Some g -> g

let merge_constraints csts g = ElimConstraints.fold enforce_constraint csts g

let check_constraint g (q1, k, q2) = check_func k g q1 q2

let check_constraints csts g = ElimConstraints.for_all (check_constraint g) csts

exception AlreadyDeclared = G.AlreadyDeclared
let add_quality g q =
  let g = G.add q g in (* Should it fail? Yes *)
  enforce_constraint (qtype, ElimConstraint.ElimTo, q) g

let enforce_eliminates_to s1 s2 g =
  enforce_constraint (s1, ElimConstraint.ElimTo, s2) g

let enforce_eq s1 s2 g =
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
let is_allowed_elimination = check_func ElimConstraint.ElimTo

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

let explain_quality_inconsistency defprv (prv, (k, q1, q2, msg)) =
  let open Pp in
  let prv = match prv with None -> defprv | Some prv -> prv in
  let msg = match msg with None -> mt () | Some m -> m in
  str "Cannot enforce" ++ spc() ++ Quality.pr prv q1 ++ spc() ++
    ElimConstraint.pr_kind k ++ spc() ++ Quality.pr prv q2 ++ spc() ++ msg
