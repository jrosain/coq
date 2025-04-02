(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** This file defines universe unification states which are part of evarmaps.
    Most of the API below is reexported in {!Evd}. Consider using higher-level
    primitives when needed. *)

open Names
open Univ
open Sorts

type universes_entry =
| Monomorphic_entry of Univ.ContextSet.t
| Polymorphic_entry of UVars.UContext.t

exception UniversesDiffer

type t
(** Type of universe unification states. They allow the incremental building of
    universe constraints during an interactive proof. *)

(** {5 Constructors} *)

(** Different ways to create a new universe state *)

val empty : t

val make : UGraph.t -> t
[@@ocaml.deprecated "(8.13) Use from_env"]

val make_with_initial_binders : UGraph.t -> lident list -> t
[@@ocaml.deprecated "(8.13) Use from_env"]

val from_env : ?binders:lident list -> Environ.env -> t
(** Main entry point at the beginning of a declaration declaring the
    binding names as rigid universes. *)

val of_names : (UnivNames.universe_binders * UnivNames.rev_binders) -> t
(** Main entry point when only names matter, e.g. for printing. *)

val of_context_set : UnivGen.sort_context_set -> t
(** Main entry point when starting from the instance of a global
    reference, e.g. when building a scheme. *)

(** Misc *)

val is_empty : t -> bool

val union : t -> t -> t

(** {5 Projections and other destructors} *)

val context_set : t -> Univ.ContextSet.t
(** The local context of the state, i.e. a set of bound variables together
    with their associated constraints. *)

val sort_context_set : t -> UnivGen.sort_context_set

type universe_opt_subst = UnivFlex.t
(* Reexport because UnivSubst is private *)

val subst : t -> UnivFlex.t
(** The local universes that are unification variables *)

val nf_universes : t -> Constr.t -> Constr.t
(** Apply the local substitution [subst] *)

val ugraph : t -> UGraph.t
(** The current graph extended with the local constraints *)

val elim_graph : t -> QGraph.t
(** The elimination graph for above prop variables *)

val eliminates_to_prop : t -> Quality.QVar.t -> bool

val is_algebraic : Level.t -> t -> bool
(** Can this universe be instantiated with an algebraic
    universe (ie it appears in inferred types only). *)

val constraints : t -> Univ.UnivConstraints.t
(** Shorthand for {!context_set} composed with {!ContextSet.constraints}. *)

val context : t -> UVars.UContext.t
(** Shorthand for {!context_set} with {!Context_set.to_context}. *)

type named_universes_entry = universes_entry * UnivNames.universe_binders

val univ_entry : poly:bool -> t -> named_universes_entry
(** Pick from {!context} or {!context_set} based on [poly]. *)

val universe_binders : t -> UnivNames.universe_binders
(** Return local names of universes. *)

val compute_instance_binders : t -> UVars.Instance.t -> UVars.bound_names

val nf_qvar : t -> Quality.QVar.t -> Quality.t
(** Returns the normal form of the sort variable. *)

val nf_quality : t -> Quality.t -> Quality.t

val nf_instance : t -> UVars.Instance.t -> UVars.Instance.t

val nf_level : t -> Level.t -> Level.t
(** Must not be allowed to be algebraic *)

val nf_universe : t -> Universe.t -> Universe.t

val nf_sort : t -> Sorts.t -> Sorts.t
(** Returns the normal form of the sort. *)

val nf_relevance : t -> relevance -> relevance
(** Returns the normal form of the relevance. *)

(** {5 UnivConstraints handling} *)

val add_constraints : t -> Univ.UnivConstraints.t -> t
(**
  @raise UniversesDiffer when universes differ
*)

val add_universe_constraints : t -> UnivProblem.Set.t -> t
(**
  @raise UniversesDiffer when universes differ
*)

val check_elim_constraints : t -> Quality.ElimConstraints.t -> bool

val check_universe_constraints : t -> UnivProblem.Set.t -> bool

val add_quconstraints : t -> QUConstraints.t -> t

(** {5 Names} *)

val quality_of_name : t -> Id.t -> Quality.QVar.t

val universe_of_name : t -> Id.t -> Univ.Level.t
(** Retrieve the universe associated to the name. *)


val name_level : Univ.Level.t -> Id.t -> t -> t
(** Gives a name to the level (making it a binder).
    Asserts the name is not already used by a level *)

(** {5 Unification} *)

(** [restrict_universe_context (univs,csts) keep] restricts [univs] to
   the universes in [keep]. The constraints [csts] are adjusted so
   that transitive constraints between remaining universes (those in
   [keep] and those not in [univs]) are preserved. *)
val restrict_universe_context : ContextSet.t -> Level.Set.t -> ContextSet.t

(** [restrict uctx ctx] restricts the local universes of [uctx] to
   [ctx] extended by local named universes and side effect universes
   (from [demote_seff_univs]). Transitive constraints between retained
   universes are preserved. *)
val restrict : t -> Univ.Level.Set.t -> t


(** [restrict_even_binders uctx ctx] restricts the local universes of [uctx] to
   [ctx] extended by side effect universes
   (from [demote_seff_univs]). Transitive constraints between retained
   universes are preserved. *)
val restrict_even_binders : t -> Univ.Level.Set.t -> t

type rigid =
  | UnivRigid
  | UnivFlexible of bool (** Is substitution by an algebraic ok? *)

val univ_rigid : rigid
val univ_flexible : rigid
val univ_flexible_alg : rigid

val merge : ?loc:Loc.t -> sideff:bool -> rigid -> t -> Univ.ContextSet.t -> t
val merge_sort_variables : ?loc:Loc.t -> sideff:bool -> t -> Quality.QVar.Set.t -> t
val merge_sort_context : ?loc:Loc.t -> sideff:bool -> rigid -> t -> UnivGen.sort_context_set -> t

val demote_global_univs : Univ.ContextSet.t -> t -> t
(** After declaring global universes, call this if you want to keep using the UState.

    Removes from the uctx_local part of the UState the universes
    that are present in the input constraint set (supposedly the global ones),
    and adds any new universes and constraints to the UGraph part of the UState.
*)

val demote_global_univ_entry : universes_entry -> t -> t
(** After declaring a global, call this with its universe entry
    if you want to keep using the ustate instead of restarting it
    with [from_env (Global.env())] or using the slow
    [update_sigma_univs _ (Environ.universes (Global/env()))].

    Equivalently:
    - In the monomorphic case, call [demote_global_univs] on the contextset.
    - In the polymorphic case, do nothing.
*)

val emit_side_effects : Safe_typing.private_constants -> t -> t
(** Calls [demote_global_univs] for the private constant universes. *)

val new_sort_variable : ?loc:Loc.t -> ?name:Id.t -> t -> t * Quality.QVar.t
(** Declare a new local sort. *)

val new_univ_variable : ?loc:Loc.t -> rigid -> Id.t option -> t -> t * Univ.Level.t
(** Declare a new local universe; use rigid if a global or bound
    universe; use flexible for a universe existential variable; use
    univ_flexible_alg for a universe existential variable allowed to
    be instantiated with an algebraic universe *)

val add_forgotten_univ : t -> Univ.Level.t -> t
(** Don't use this, it only exists for funind *)

val make_nonalgebraic_variable : t -> Univ.Level.t -> t
(** cf UnivFlex *)

val make_flexible_nonalgebraic : t -> t
(** cf UnivFlex *)

val normalize_variables : t -> t

val constrain_variables : Univ.Level.Set.t -> t -> t

val fix_undefined_variables : t -> t
(** cf UnivFlex *)

(** Universe minimization *)
val minimize : t -> t

val collapse_elim_to_prop_sort_variables : to_prop:bool -> t -> t

val collapse_sort_variables : ?except:Quality.QVar.Set.t -> t -> t

type ('a, 'b, 'c, 'd) gen_poly_decl = {
  polydecl_qualities : 'a;
  polydecl_extensible_qualities : bool;
  polydecl_elim_constraints : 'b;
  polydecl_extensible_elim_constraints : bool;
  polydecl_instance : 'c; (* Declared universes *)
  polydecl_extensible_instance : bool; (* Can new universes be added *)
  polydecl_univ_constraints : 'd; (* Declared univ constraints *)
  polydecl_extensible_univ_constraints : bool (* Can new constraints be added *) }

type poly_decl =
  (Quality.QVar.t list, Quality.ElimConstraints.t, Level.t list, Univ.UnivConstraints.t) gen_poly_decl

val default_poly_decl : poly_decl

(** [check_poly_decl ctx decl]

   If non extensible in [decl], check that the local universes (resp.
   universe constraints) in [ctx] are implied by [decl].

   Return a [universes_entry] containing the local
   universes of [ctx] and their constraints.

   When polymorphic, the universes corresponding to
   [decl.univdecl_instance] come first in the order defined by that
   list. *)
val check_poly_decl : poly:bool -> t -> poly_decl -> named_universes_entry
val check_poly_decl_rev : t -> poly_decl -> t * UVars.UContext.t
val check_uctx_impl : fail:(Pp.t -> unit) -> t -> t -> unit

val check_mono_poly_decl : t -> poly_decl -> Univ.ContextSet.t

val check_template_poly_decl : t -> template_qvars:Quality.QVar.Set.t -> poly_decl -> Univ.ContextSet.t

(** {5 TODO: Document me} *)

val update_sigma_univs : t -> UGraph.t -> t

(** {5 Pretty-printing} *)

val pr_uctx_level : t -> Univ.Level.t -> Pp.t
val pr_uctx_qvar : t -> Quality.QVar.t -> Pp.t
val qualid_of_level : t -> Univ.Level.t -> Libnames.qualid option

(** Only looks in the local names, not in the nametab. *)
val id_of_level : t -> Univ.Level.t -> Id.t option

val id_of_qvar : t -> Quality.QVar.t -> Id.t option

val is_rigid_qvar : t -> Quality.QVar.t -> bool

val pr_weak : (Univ.Level.t -> Pp.t) -> t -> Pp.t

val pr : t -> Pp.t

val pr_sort_opt_subst : t -> Pp.t

module Internal :
sig

val reboot : Environ.env -> t -> t
(** Madness-inducing hack dedicated to the handling of universes of Program.
    DO NOT USE OUTSIDE OF DEDICATED AREA. *)

end
