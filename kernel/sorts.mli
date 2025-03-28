(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** {6 The sorts of CCI. } *)

type t = private
  | SProp
  | Prop
  | Set
  | Type of Univ.Universe.t
  | QSort of Quality.QVar.t * Univ.Universe.t

val sprop : t
val set  : t
val prop : t
val type1  : t
val qsort : Quality.QVar.t -> Univ.Universe.t -> t
val make : Quality.t -> Univ.Universe.t -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val is_sprop : t -> bool
val is_set : t -> bool
val is_prop : t -> bool
val is_small : t -> bool
val quality : t -> Quality.t

val hcons : t Hashcons.f

val sort_of_univ : Univ.Universe.t -> t
val univ_of_sort : t -> Univ.Universe.t

val levels : t -> Univ.Level.Set.t

val super : t -> t

val subst_fn : (Quality.QVar.t -> Quality.t) * (Univ.Universe.t -> Univ.Universe.t) -> t -> t

(** On binders: is this variable proof relevant *)
(* TODO put in submodule or new file *)
type relevance = Relevant | Irrelevant | RelevanceVar of Quality.QVar.t

val relevance_hash : relevance -> int

val relevance_equal : relevance -> relevance -> bool

val relevance_subst_fn : (Quality.QVar.t -> Quality.t) -> relevance -> relevance

val relevance_of_sort : t -> relevance

val debug_print : t -> Pp.t
val pr : (Quality.QVar.t -> Pp.t) -> (Univ.Universe.t -> Pp.t) -> t -> Pp.t
val raw_pr : t -> Pp.t

type pattern =
  | PSProp | PSSProp | PSSet | PSType of int option | PSQSort of int option * int option

val pattern_match : pattern -> t -> ('t, Quality.t, Univ.Level.t) Partial_subst.t -> ('t, Quality.t, Univ.Level.t) Partial_subst.t option

val enforce_eq_quality : Quality.t -> Quality.t -> Quality.ElimConstraints.t -> Quality.ElimConstraints.t

val enforce_elim_to_quality : Quality.t -> Quality.t -> Quality.ElimConstraints.t -> Quality.ElimConstraints.t

module QUConstraints : sig
  type t = Quality.ElimConstraints.t * Univ.Constraints.t

  val empty : t

  val union : t -> t -> t
end
