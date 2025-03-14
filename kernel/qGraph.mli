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

(** {6 Graphs of quality elimination constraints. } *)

type t

exception QualitityInconsistency of string (* FIXME : something else *)

exception AlreadyDeclared
val add_quality : t -> quality -> t
(** Add a quality to the graph. Enforces Type to be eliminable to the new quality *)

val merge_constraints : ElimConstraints.t -> t -> t

val check_constraints : ElimConstraints.t -> t -> bool

val enforce_eliminates_to : quality -> quality -> t -> t
(** Checks whether the first quality eliminates to the second. If it's
    consistent within the graph, then adds the constraint. *)

val enforce_eq : t -> quality -> quality -> t
(** Checks whether the first quality is equal to the second. If it's
    consistent within the graph, then adds the constraint. *)

val initial_quality_constraints : t
(** Initial graph of quality elimination constraints. *)

val is_allowed_elimination : t -> quality -> quality -> bool
(** Check that the first quality is eliminable into the second one. *)

val sort_eliminates_to : t -> Sorts.t -> Sorts.t -> bool

val check_eq : t -> quality -> quality -> bool

val check_eq_sort : t -> Sorts.t -> Sorts.t -> bool

val eliminates_to_prop : t -> quality -> bool

val domain : t -> Set.t
(** Return the set of qualities. *)

val qvar_domain : t -> QVar.Set.t
(** Return the set of quality variables. *)

val is_empty : t -> bool
(** Check that the graph is empty. *)

val add_template_qvars : QVar.Set.t -> t -> t
(** Set all the qvars in the set to eliminate to Prop.
    Do not use outside kernel inductive typechecking. *)
