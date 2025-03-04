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

val initial_quality_constraints : t
(** Initial graph of quality elimination constraints. *)

val is_allowed_elimination : t -> Quality.t -> Quality.t -> bool
(** Check that the first quality is eliminable into the second one. *)