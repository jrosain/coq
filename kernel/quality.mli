(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** {6 The sort qualities of CCI. } *)

module QVar :
sig
  type t

  val var_index : t -> int option

  val make_var : int -> t
  val make_unif : string -> int -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val hash : t -> int

  val raw_pr : t -> Pp.t
  (** Using this is incorrect when names are available, typically from an evar map. *)

  val to_string : t -> string
  (** Debug printing *)

  type repr =
    | Var of int
    | Unif of string * int

  val repr : t -> repr
  val of_repr : repr -> t

  module Set : CSig.SetS with type elt = t

  module Map : CMap.ExtS with type key = t and module Set := Set
end

type constant = QProp | QSProp | QType
type t = QVar of QVar.t | QConstant of constant

type quality = t

module Constants : sig
  val equal : constant -> constant -> bool
  val compare : constant -> constant -> int
  val eliminates_to : constant -> constant -> bool
  val to_string : constant -> string
  val pr : constant -> Pp.t
end

val qprop : t
val qsprop : t
val qtype : t

val var : int -> t
(** [var i] is [QVar (QVar.make_var i)] *)

val var_index : t -> int option

val equal : t -> t -> bool

val is_qsprop : t -> bool
val is_qprop : t -> bool
val is_qtype : t -> bool
val is_qvar : t -> bool

val compare : t -> t -> int

val eliminates_to : t -> t -> bool

val all_constants : t list
val all : t list
(** This provides a list with all qualities, and a dummy QVar. *)

val pr : (QVar.t -> Pp.t) -> t -> Pp.t

val raw_pr : t -> Pp.t

val hash : t -> int

val hcons : t -> t

(* XXX Inconsistent naming: this one should be subst_fn *)
val subst : (QVar.t -> t) -> t -> t

val subst_fn : t QVar.Map.t -> QVar.t -> t

module Set : CSig.SetS with type elt = t
module Map : CMap.ExtS with type key = t and module Set := Set

type pattern =
  PQVar of int option | PQConstant of constant

val pattern_match : pattern -> t -> ('t, t, 'u) Partial_subst.t -> ('t, t, 'u) Partial_subst.t option

val to_string : t -> string

module ElimConstraint : sig
  type kind = Eq | ElimTo | SElimTo

  val pr_kind : kind -> Pp.t

  type t = quality * kind * quality

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val trivial : t -> bool

  val pr : (QVar.t -> Pp.t) -> t -> Pp.t

  val raw_pr : t -> Pp.t
end

module ElimConstraints : sig
  include Stdlib.Set.S with type elt = ElimConstraint.t
  val trivial : t -> bool

  val pr : (QVar.t -> Pp.t) -> t -> Pp.t
end

module HElimConstraints : sig
  include Hashcons.S with
    type t = ElimConstraints.t and
    type u = ElimConstraint.t -> ElimConstraint.t
end

val hcons_elim_constraints : HElimConstraints.t -> HElimConstraints.t
