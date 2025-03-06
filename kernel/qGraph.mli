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

(** {6 Graphs of quality elimination constraints. } *)

type t

exception QualitityInconsistency of string (* FIXME : something else *)

val add_quality : t -> Quality.t -> t
(** Add a quality to the graph. Enforces Type to be eliminable to the new quality *)

val enforce_eliminates_to : t -> Quality.t -> Quality.t -> t
(** Checks whether the first quality eliminates to the second. If it's
    consistent within the graph, then adds the constraint. *)

val enforce_eq : t -> Quality.t -> Quality.t -> t
(** Checks whether the first quality is equal to the second. If it's
    consistent within the graph, then adds the constraint. *)

val initial_quality_constraints : t
(** Initial graph of quality elimination constraints. *)

val is_allowed_elimination : t -> Quality.t -> Quality.t -> bool
(** Check that the first quality is eliminable into the second one. *)

val domain : t -> Quality.Set.t
(** Return the set of qualities. *)

val qvar_domain : t -> QVar.Set.t
(** Return the set of quality variables. *)

val is_empty : t -> bool
(** Check that the graph is empty. *)
