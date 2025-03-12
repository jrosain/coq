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
open PolyConstraints

(** {6 Support for universe polymorphism } *)

module Variance :
sig
  (** A universe position in the instance given to a cumulative
     inductive can be the following. Note there is no Contravariant
     case because [forall x : A, B <= forall x : A', B'] requires [A =
     A'] as opposed to [A' <= A]. *)
  type t = Irrelevant | Covariant | Invariant

  (** [check_subtype x y] holds if variance [y] is also an instance of [x] *)
  val check_subtype : t -> t -> bool

  val sup : t -> t -> t

  val pr : t -> Pp.t

  val equal : t -> t -> bool

end

(** {6 Universe instances} *)

module Instance :
sig
  type t
  (** A universe instance represents a vector of argument universes
      and sort qualities to a polymorphic definition
      (constant, inductive or constructor). *)

  val empty : t
  val is_empty : t -> bool

  val of_array : quality array * Level.t array -> t
  val to_array : t -> quality array * Level.t array

  val abstract_instance : int * int -> t
  (** Instance of the given size made of QVar/Level.var *)

  val append : t -> t -> t
  (** To concatenate two instances, used for discharge *)

  val equal : t -> t -> bool
  (** Equality *)

  val length : t -> int * int
  (** Instance length *)

  val hcons : t -> t
  (** Hash-consing. *)

  val hash : t -> int
  (** Hash value *)

  val share : t -> t * int
  (** Simultaneous hash-consing and hash-value computation *)

  val pr : (QVar.t -> Pp.t) -> (Level.t -> Pp.t) -> ?variance:Variance.t array -> t -> Pp.t
  (** Pretty-printing, no comments *)

  val levels : t -> Quality.Set.t * Level.Set.t
  (** The set of levels in the instance *)

  val subst_fn
    : (QVar.t -> quality) * (Level.t -> Level.t)
    -> t -> t

  type mask = Quality.pattern array * int option array

  val pattern_match : mask -> t -> ('term, quality, Level.t) Partial_subst.t -> ('term, quality, Level.t) Partial_subst.t option
  (** Pattern matching, as used by the rewrite rules mechanism *)
end

val eq_sizes : int * int -> int * int -> bool
(** Convenient function to compare the result of Instance.length, UContext.size etc *)

val enforce_eq_instances : Instance.t constraint_function

val enforce_eq_variance_instances : Variance.t array -> Instance.t constraint_function
val enforce_leq_variance_instances : Variance.t array -> Instance.t constraint_function

type 'a puniverses = 'a * Instance.t
val out_punivs : 'a puniverses -> 'a
val in_punivs : 'a -> 'a puniverses

val eq_puniverses : ('a -> 'a -> bool) -> 'a puniverses -> 'a puniverses -> bool

type bound_names =
  { qualities : Names.Name.t array
  ; levels : Names.Name.t array }

(** A vector of universe levels with universe PolyConstraints.t,
    representing local universe variables and associated PolyConstraints.t;
    the names are user-facing names for printing *)

module PolyContext :
sig
  type t

  val make : bound_names -> Instance.t constrained -> t

  val empty : t
  val is_empty : t -> bool

  val instance : t -> Instance.t
  val constraints : t -> PolyConstraints.t

  val union : t -> t -> t
  (** Keeps the order of the instances *)

  val size : t -> int * int
  (** The number of universes in the context *)

  val names : t -> bound_names
  (** Return the user names of the universes *)

  val refine_names : bound_names -> t -> t
  (** Use names to name the possibly yet unnamed universes *)

  val sort_levels : Level.t array -> Level.t array
  (** Arbitrary choice of linear order of the variables *)

  val sort_qualities : quality array -> quality array
  (** Arbitrary choice of linear order of the variables *)

  val of_context_set : (Instance.t -> bound_names) -> QVar.Set.t -> ContextSet.t -> t
  (** Build a vector of universe levels assuming a function generating names *)

  val to_context_set : t -> QVar.Set.t * ContextSet.t
  (** Discard the names and order of the universes *)

end
(** A value in a universe context. *)
type 'a in_poly_context = 'a * PolyContext.t

module AbstractContext :
sig
  type t
  (** An abstract context serves to quantify over a graph of universes
      represented using de Bruijn indices, as in:
      u0, ..., u(n-1), Var i < Var j, .., Var k <= Var l |- term(Var 0 .. Var (n-1))
      \-------------/  \-------------------------------/    \---------------------/
      names for        constraints expressed on de Bruijn   judgement in abstract
      printing         representation of the n univ vars    context expected to
                                                            use de Bruijn indices
  *)

  val make : bound_names -> PolyConstraints.t -> t
  (** Build an abstract context. PolyConstraints may be between universe
     variables. *)

  val repr : t -> PolyContext.t
  (** [repr ctx] is [(Var(0), ... Var(n-1) |= cstr] where [n] is the length of
      the context and [cstr] the abstracted PolyConstraints.t. *)

  val empty : t
  val is_empty : t -> bool

  val is_constant : t -> bool
  (** Empty instance, but may have constraints *)

  val size : t -> int * int

  val union : t -> t -> t
  (** The constraints are expected to be relative to the concatenated set of universes *)

  val instantiate : Instance.t -> t -> PolyConstraints.t
  (** Generate the set of instantiated PolyConstraints.t **)

  val names : t -> bound_names
  (** Return the names of the bound universe variables *)

end

type 'a univ_abstracted = {
  univ_abstracted_value : 'a;
  univ_abstracted_binder : AbstractContext.t;
}
(** A value with bound universe levels. *)

val map_univ_abstracted : ('a -> 'b) -> 'a univ_abstracted -> 'b univ_abstracted

(** {6 Substitution} *)

type universe_level_subst = Level.t Level.Map.t

val empty_level_subst : universe_level_subst
val is_empty_level_subst : universe_level_subst -> bool

(** Substitution of universes. *)
val subst_univs_level_level : universe_level_subst -> Level.t -> Level.t
val subst_univs_level_universe : universe_level_subst -> Universe.t -> Universe.t
val subst_univs_level_constraints : universe_level_subst -> PolyConstraints.t -> PolyConstraints.t

val pr_universe_level_subst : (Level.t -> Pp.t) -> universe_level_subst -> Pp.t

val pr_quality_level_subst : (QVar.t -> Pp.t) -> quality QVar.Map.t -> Pp.t

type sort_level_subst = quality QVar.Map.t * universe_level_subst

val empty_sort_subst : sort_level_subst

val is_empty_sort_subst : sort_level_subst -> bool

val subst_univs_level_abstract_universe_context :
  universe_level_subst -> AbstractContext.t -> AbstractContext.t
(** There are no constraints on qualities, so this only needs a subst for univs *)

val subst_sort_level_instance : sort_level_subst -> Instance.t -> Instance.t
(** Level to universe substitutions. *)

val subst_sort_level_quality : sort_level_subst -> quality -> quality

val subst_sort_level_sort : sort_level_subst -> Sorts.t -> Sorts.t

val subst_sort_level_relevance : sort_level_subst -> Sorts.relevance -> Sorts.relevance

(** Substitution of instances *)
val subst_instance_instance : Instance.t -> Instance.t -> Instance.t
val subst_instance_universe : Instance.t -> Universe.t -> Universe.t
val subst_instance_quality : Instance.t -> quality -> quality
val subst_instance_sort : Instance.t -> Sorts.t -> Sorts.t
val subst_instance_relevance : Instance.t -> Sorts.relevance -> Sorts.relevance
val subst_instance_sort_level_subst : Instance.t -> sort_level_subst -> sort_level_subst

val make_instance_subst : Instance.t -> sort_level_subst
(** Creates [u(0) ↦ 0; ...; u(n-1) ↦ n - 1] out of [u(0); ...; u(n - 1)] *)

val abstract_universes : PolyContext.t -> Instance.t * AbstractContext.t
(** TODO: move universe abstraction out of the kernel *)

val make_abstract_instance : AbstractContext.t -> Instance.t

(** {6 Pretty-printing of universes. } *)

val pr_poly_context : (QVar.t -> Pp.t) -> (Level.t -> Pp.t) -> ?variance:Variance.t array ->
  PolyContext.t -> Pp.t
val pr_abstract_context : (QVar.t -> Pp.t) -> (Level.t -> Pp.t) -> ?variance:Variance.t array ->
  AbstractContext.t -> Pp.t

(** {6 Hash-consing } *)

val hcons_poly_context : PolyContext.t -> PolyContext.t
val hcons_abstract_universe_context : AbstractContext.t -> AbstractContext.t
