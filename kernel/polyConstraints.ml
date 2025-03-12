(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Univ
open Quality

type t = ElimConstraints.t * LvlConstraints.t

type poly_constraints = t

let make q u = (q, u)

let qualities = fst

let levels = snd

let add_quality q (qc, lc) = (ElimConstraints.add q qc, lc)

let add_level u (qc, lc) = (qc, LvlConstraints.add u lc)

let empty = (ElimConstraints.empty, LvlConstraints.empty)

let is_empty (qc, lc) =
  ElimConstraints.is_empty qc && LvlConstraints.is_empty lc

let equal (qc, lc) (qc', lc') =
  ElimConstraints.equal qc qc' && LvlConstraints.equal lc lc'

let union (qc, lc) (qc', lc') =
  (ElimConstraints.union qc qc', LvlConstraints.union lc lc')

let fold (qf, lf) (qc, lc) (x, y) =
  (ElimConstraints.fold qf qc x, LvlConstraints.fold lf lc y)

(* let add (q, l) (qc, lc) = *)
(*   (ElimConstraints.add q qc, LvlConstraints.add l lc) *)

let diff (qc, lc) (qc', lc') =
  (ElimConstraints.diff qc qc', LvlConstraints.diff lc lc')

let pr prv prl (qc, lc) =
  let open Pp in
  v 0 (app (ElimConstraints.pr prv qc) (LvlConstraints.pr prl lc))

type elim_constraints_func = ElimConstraints.t -> ElimConstraints.t
type lvl_constraints_func  = LvlConstraints.t -> LvlConstraints.t

module HPolyConstraints =
  Hashcons.Make(
    struct
      type t = poly_constraints
      type u = elim_constraints_func * lvl_constraints_func
      let hashcons (qf, uf) (qc, uc) = (qf qc, uf uc)
      let eq (qc, uc) (qc', uc') =
	qc == qc' && uc == uc'
      let hash = Hashtbl.hash
    end)

let hcons_poly_constraints =
  Hashcons.simple_hcons
    HPolyConstraints.generate
    HPolyConstraints.hcons
    (hcons_elim_constraints, hcons_lvl_constraints)

(** A value with universe constraints. *)
type 'a constrained = 'a * t

let constraints_of (_, cst) = cst

(** Constraints functions. *)

type 'a constraint_function = 'a -> 'a -> t -> t

let enforce_eq_level u v c =
  (* We discard trivial constraints like u=u *)
  if Level.equal u v then c
  else add_level (u, Eq, v) c

let enforce_leq_level u v c =
  if Level.equal u v then c
  else add_level (u, Le, v) c

let enforce_eq_quality a b csts =
  if Quality.equal a b then csts
  else add_quality (a, ElimConstraint.Eq , b) csts

(* let enforce_elim_to_quality a b csts = *)
(*   if Quality.equal a b then csts *)
(*   else match a, b with *)
(*        | Quality.(QConstant _), Quality.(QConstant _) -> csts *)
(*        | _ -> add_quality (a ,ElimConstraint.ElimTo, b) csts *)

module ContextSet = struct
  type t = Level.Set.t constrained

  let of_lvl_set s : t = (s, empty)
  let singleton_lvl l = of_lvl_set (Level.Set.singleton l)

  let empty = (Level.Set.empty, empty)
  let is_empty (univs, cst) = Level.Set.is_empty univs && is_empty cst

  let equal (univs, cst as x) (univs', cst' as y) =
    x == y || (Level.Set.equal univs univs' && equal cst cst')

  let union (univs, cst as x) (univs', cst' as y) =
    if x == y then x
    else Level.Set.union univs univs', union cst cst'

  let append (univs, cst) (univs', cst') =
    let univs = Level.Set.fold Level.Set.add univs univs' in
    let cst = fold (ElimConstraints.add, LvlConstraints.add) cst cst' in
    (univs, cst)

  let diff (univs, cst) (univs', cst') =
    Level.Set.diff univs univs', diff cst cst'

  let add_level u (univs, cst) =
    Level.Set.add u univs, cst

  let add_constraints cst' (univs, cst) =
    union (univs, cst) (Level.Set.empty, cst')

  let pr prv prl (univs, cst as ctx) =
    let open Pp in
    if is_empty ctx then mt() else
      hov 0 (h (Level.Set.pr prl univs ++ str " |=") ++ brk(1,2) ++ h (pr prv prl cst))

  let constraints (_univs, cst) = cst
  let levels (univs, _cst) = univs

  let size (univs,_) = Level.Set.cardinal univs
end

type 'a in_poly_context_set = 'a * ContextSet.t

(* (\** A value in a universe context (resp. context set). *\) *)
(* type 'a in_universe_context_set = 'a * universe_context_set *)

(* (\** Pretty-printing *\) *)

(* replaced by exposure of pr in ContextSet *)
(* let pr_universe_context_set = ContextSet.pr *)

(* module Huniverse_set = *)
(*   Hashcons.Make( *)
(*     struct *)
(*       type t = universe_set *)
(*       type u = universe_level -> universe_level *)
(*       let hashcons huc s = *)
(*         Level.Set.fold (fun x -> Level.Set.add (huc x)) s Level.Set.empty *)
(*       let eq s s' = *)
(*         Level.Set.equal s s' *)
(*       let hash = Hashtbl.hash *)
(*     end) *)

(* let hcons_universe_set = *)
(*   Hashcons.simple_hcons Huniverse_set.generate Huniverse_set.hcons Level.hcons *)

(* let hcons_universe_context_set (v, c) = *)
(*   (hcons_universe_set v, hcons_constraints c) *)

(* let hcons_univ x = Universe.hcons x *)
