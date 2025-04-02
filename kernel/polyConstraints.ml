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

type t = ElimConstraints.t * UnivConstraints.t

type poly_constraints = t

let make q u = (q, u)

let qualities = fst

let univs = snd

let add_quality q (qc, lc) = (ElimConstraints.add q qc, lc)

let add_univ u (qc, lc) = (qc, UnivConstraints.add u lc)

let of_qualities qc = make qc UnivConstraints.empty

let of_univs lc = make ElimConstraints.empty lc

let set_qualities qc (_,lc) = make qc lc

let set_univs lc (qc,_) = make qc lc

let empty = (ElimConstraints.empty, UnivConstraints.empty)

let is_empty (qc, lc) =
  ElimConstraints.is_empty qc && UnivConstraints.is_empty lc

let equal (qc, lc) (qc', lc') =
  ElimConstraints.equal qc qc' && UnivConstraints.equal lc lc'

let union (qc, lc) (qc', lc') =
  (ElimConstraints.union qc qc', UnivConstraints.union lc lc')

let fold (qf, lf) (qc, lc) (x, y) =
  (ElimConstraints.fold qf qc x, UnivConstraints.fold lf lc y)

let diff (qc, lc) (qc', lc') =
  (ElimConstraints.diff qc qc', UnivConstraints.diff lc lc')

let elements (qc, lc) =
  (ElimConstraints.elements qc, UnivConstraints.elements lc)

let filter_univs f (qc, lc) =
  make qc @@ UnivConstraints.filter f lc

let pr prv prl (qc, lc) =
  let open Pp in
  let ppelim = if ElimConstraints.is_empty qc then mt()
               else ElimConstraints.pr prv qc ++ str"," in
  v 0 (ppelim ++ UnivConstraints.pr prl lc)

module HPolyConstraints =
  Hashcons.Make(
    struct
      type t = poly_constraints
      let hashcons (qf, uf) =
        let hqf, qf = ElimConstraints.hcons qf in
        let huf, uf = UnivConstraints.hcons uf in
        Hashset.Combine.(combine hqf huf), (qf, uf)
      let eq (qc, uc) (qc', uc') =
        qc == qc' && uc == uc'
    end)

let hcons =
  Hashcons.simple_hcons
    HPolyConstraints.generate
    HPolyConstraints.hcons ()

(** A value with universe constraints. *)
type 'a constrained = 'a * t

let constraints_of (_, cst) = cst

(** Constraints functions. *)

type 'a constraint_function = 'a -> 'a -> t -> t

let enforce_eq_univ u v c =
  (* We discard trivial constraints like u=u *)
  if Level.equal u v then c
  else add_univ (u, UnivConstraint.Eq, v) c

let enforce_leq_univ u v c =
  if Level.equal u v then c
  else add_univ (u, UnivConstraint.Le, v) c

let enforce_eq_quality q1 q2 csts =
  if Quality.equal q1 q2 then csts
  else add_quality (q1, ElimConstraint.Equal, q2) csts

let enforce_elim_to q1 q2 csts =
  if QGraph.ElimTable.eliminates_to q1 q2 then csts
  else add_quality (q1, ElimConstraint.ElimTo, q2) csts
