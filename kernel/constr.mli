(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** This file defines the most important datatype of Rocq, namely kernel terms,
    as well as a handful of generic manipulation functions. *)

open Names

(** {6 Simply type aliases } *)
type pconstant = Constant.t UVars.puniverses
type pinductive = inductive UVars.puniverses
type pconstructor = constructor UVars.puniverses

(** {6 Existential variables } *)
type metavariable = int

(** {6 Case annotation } *)
type case_style = LetStyle | IfStyle | LetPatternStyle | MatchStyle
  | RegularStyle (** infer printing form from number of constructor *)
type case_printing =
  { style     : case_style }

(* INVARIANT:
 * - Array.length ci_cstr_ndecls = Array.length ci_cstr_nargs
 * - forall (i : 0 .. pred (Array.length ci_cstr_ndecls)),
 *          ci_cstr_ndecls.(i) >= ci_cstr_nargs.(i)
 *)
type case_info =
  { ci_ind        : inductive;      (* inductive type to which belongs the value that is being matched *)
    ci_npar       : int;            (* number of parameters of the above inductive type *)
    ci_cstr_ndecls : int array;     (* For each constructor, the corresponding integer determines
                                       the number of values that can be bound in a match-construct.
                                       NOTE: parameters of the inductive type are therefore excluded from the count *)
    ci_cstr_nargs : int array;      (* for each constructor, the corresponding integers determines
                                       the number of values that can be applied to the constructor,
                                       in addition to the parameters of the related inductive type
                                       NOTE: "lets" are therefore excluded from the count
                                       NOTE: parameters of the inductive type are also excluded from the count *)
    ci_pp_info    : case_printing   (* not interpreted by the kernel *)
  }

(** {6 The type of constructions } *)

type t
type constr = t
(** [types] is the same as [constr] but is intended to be used for
   documentation to indicate that such or such function specifically works
   with {e types} (i.e. terms of type a sort).
   (Rem:plurial form since [type] is a reserved ML keyword) *)

type types = constr

(** {5 Functions for dealing with constr terms. }
  The following functions are intended to simplify and to uniform the
  manipulation of terms. Some of these functions may be overlapped with
  previous ones. *)

(** {6 Term constructors. } *)

(** Constructs a de Bruijn index (DB indices begin at 1) *)
val mkRel : int -> constr

(** Constructs a Variable *)
val mkVar : Id.t -> constr

(** Constructs a machine integer *)
val mkInt : Uint63.t -> constr

(** Constructs an array *)
val mkArray : UVars.Instance.t * constr array * constr * types -> constr

(** Constructs a machine float number *)
val mkFloat : Float64.t -> constr

(** Constructs a machine string. *)
val mkString : Pstring.t -> constr

(** Constructs an patvar named "?n" *)
val mkMeta : metavariable -> constr

(** Constructs an existential variable *)
type existential = Evar.t * constr SList.t
val mkEvar : existential -> constr

(** Construct a sort *)
val mkSort : Sorts.t -> types
val mkSProp : types
val mkProp : types
val mkSet  : types
val mkType : Univ.Universe.t -> types


(** This defines the strategy to use for verifiying a Cast *)
type cast_kind = VMcast | NATIVEcast | DEFAULTcast

(** Constructs the term [t1::t2], i.e. the term t{_ 1} casted with the
   type t{_ 2} (that means t2 is declared as the type of t1). *)
val mkCast : constr * cast_kind * constr -> constr

type 'a binder_annot = ('a,Sorts.relevance) Context.pbinder_annot

(** Constructs the product [(x:t1)t2] *)
val mkProd : Name.t binder_annot * types * types -> types

(** Constructs the abstraction \[x:t{_ 1}\]t{_ 2} *)
val mkLambda : Name.t binder_annot * types * constr -> constr

(** Constructs the product [let x = t1 : t2 in t3] *)
val mkLetIn : Name.t binder_annot * constr * types * constr -> constr

(** [mkApp (f, [|t1; ...; tN|]] constructs the application
    {%html:(f t<sub>1</sub> ... t<sub>n</sub>)%}
    {%latex:$(f~t_1\dots f_n)$%}. *)
val mkApp : constr * constr array -> constr

val map_puniverses : ('a -> 'b) -> 'a UVars.puniverses -> 'b UVars.puniverses

(** Constructs a Constant.t *)
val mkConstU : pconstant -> constr

(** Constructs a projection application *)
val mkProj : (Projection.t * Sorts.relevance * constr) -> constr

(** Inductive types *)

(** Constructs the ith (co)inductive type of the block named kn *)
val mkIndU : pinductive -> constr

(** Constructs the jth constructor of the ith (co)inductive type of the
   block named kn. *)
val mkConstructU : pconstructor -> constr
val mkConstructUi : pinductive * int -> constr

(** Make a constant, inductive, constructor or variable. *)
val mkRef : GlobRef.t UVars.puniverses -> constr

module UnsafeMonomorphic : sig
  val mkConst : Constant.t -> constr
  val mkInd : inductive -> constr
  val mkConstruct : constructor -> constr
end

(** Constructs a destructor of inductive type.

    [mkCase ci params p c ac] stand for match [c] as [x] in [I args] return [p] with [ac]
    presented as describe in [ci].


    [p] structure is [args x |- "return clause"]

    [ac]{^ ith} element is ith constructor case presented as
    {e construct_args |- case_term } *)

type ('constr,'r) pcase_branch = (Name.t,'r) Context.pbinder_annot array * 'constr
(** Names bound by matching the constructor for this branch. *)

type ('types,'r) pcase_return = ((Name.t,'r) Context.pbinder_annot array * 'types) * 'r
(** Names of the indices + name of self *)

type 'constr pcase_invert =
  | NoInvert
  (** Normal reduction: match when the scrutinee is a constructor. *)

  | CaseInvert of { indices : 'constr array; }
  (** SProp to non SProp only:
      No constructors or reduce when the indices match those of the unique constructor. *)

type ('constr, 'types, 'univs, 'r) pcase =
  case_info * 'univs * 'constr array * ('types,'r) pcase_return * 'constr pcase_invert * 'constr * ('constr,'r) pcase_branch array

type case_invert = constr pcase_invert
type case_return = (types, Sorts.relevance) pcase_return
type case_branch = (constr, Sorts.relevance) pcase_branch
type case = (constr, types, UVars.Instance.t, Sorts.relevance) pcase

val mkCase : case -> constr

(** If [recindxs = [|i1,...in|]]
      [funnames = [|f1,.....fn|]]
      [typarray = [|t1,...tn|]]
      [bodies   = [|b1,.....bn|]]
   then [mkFix ((recindxs,i), funnames, typarray, bodies) ]
   constructs the {% $ %}i{% $ %}th function of the block (counting from 0)

    [Fixpoint f1 [ctx1] = b1
     with     f2 [ctx2] = b2
     ...
     with     fn [ctxn] = bn.]

   where the length of the {% $ %}j{% $ %}th context is {% $ %}ij{% $ %}.
*)
type ('constr, 'types, 'r) prec_declaration =
    (Name.t, 'r) Context.pbinder_annot array * 'types array * 'constr array
type ('constr, 'types, 'r) pfixpoint =
    (int array * int) * ('constr, 'types, 'r) prec_declaration
(** The array of [int]'s tells for each component of the array of
   mutual fixpoints the number of lambdas to skip before finding the
   recursive argument (e.g., value is 2 in "fix f (x:A) (y:=t) (z:B)
   (v:=u) (w:I) {struct w}"), telling to skip x and z and that w is
   the recursive argument); The second component [int] tells which
   component of the block is returned *)

type ('constr, 'types, 'r) pcofixpoint =
    int * ('constr, 'types, 'r) prec_declaration
(** The component [int] tells which component of the block of
   cofixpoint is returned *)

type rec_declaration = (constr, types, Sorts.relevance) prec_declaration

type fixpoint = (constr, types, Sorts.relevance) pfixpoint
val mkFix : fixpoint -> constr

(** If [funnames = [|f1,.....fn|]]
      [typarray = [|t1,...tn|]]
      [bodies   = [b1,.....bn]]
   then [mkCoFix (i, (funnames, typarray, bodies))]
   constructs the ith function of the block

    [CoFixpoint f1 = b1
     with       f2 = b2
     ...
     with       fn = bn.]
 *)
type cofixpoint = (constr, types, Sorts.relevance) pcofixpoint
val mkCoFix : cofixpoint -> constr


(** {6 Concrete type for making pattern-matching. } *)

(** [constr list] is an instance matching definitional [named_context] in
   the same order (i.e. last argument first) *)
type 'constr pexistential = Evar.t * 'constr SList.t

type ('constr, 'types, 'sort, 'univs, 'r) kind_of_term =
  | Rel       of int
  (** Gallina-variable introduced by [forall], [fun], [let-in], [fix], or [cofix]. *)
  | Var       of Id.t
  (** Gallina-variable that was introduced by Vernacular-command that
     extends the local context of the currently open section (i.e.
     [Variable] or [Let]). *)
  | Meta      of metavariable
  | Evar      of 'constr pexistential
  | Sort      of 'sort
  | Cast      of 'constr * cast_kind * 'types
  | Prod      of (Name.t,'r) Context.pbinder_annot * 'types * 'types
  (** Concrete syntax ["forall A:B,C"] is represented as [Prod (A,B,C)]. *)
  | Lambda    of (Name.t,'r) Context.pbinder_annot * 'types * 'constr
  (** Concrete syntax ["fun A:B => C"] is represented as [Lambda (A,B,C)].  *)
  | LetIn     of (Name.t,'r) Context.pbinder_annot * 'constr * 'types * 'constr
  (** Concrete syntax ["let A:C := B in D"] is represented as [LetIn (A,B,C,D)]. *)
  | App       of 'constr * 'constr array
  (** Concrete syntax ["(F P1 P2 ...  Pn)"] is represented as [App (F, [|P1; P2; ...; Pn|])].
      The {!mkApp} constructor also enforces the following invariant:
      - [F] itself is not {!App}
      - and [[|P1;..;Pn|]] is not empty. *)
  | Const     of (Constant.t * 'univs)
  (** Gallina-variable that was introduced by Vernacular-command that
     extends the global environment (i.e. [Parameter], or [Axiom], or
     [Definition], or [Theorem], or [Symbol] etc.) *)
  | Ind       of (inductive * 'univs)
  (** A name of an inductive type defined by [Variant], [Inductive] or
     [Record] Vernacular-commands. *)
  | Construct of (constructor * 'univs)
  (** A constructor of an inductive type defined by [Variant],
     [Inductive] or [Record] Vernacular-commands. *)
  | Case      of case_info * 'univs * 'constr array * ('types,'r) pcase_return * 'constr pcase_invert * 'constr * ('constr,'r) pcase_branch array
  (** [Case (ci,u,params,p,iv,c,brs)] is a [match c return p with brs]
     expression. [c] lives in inductive [ci.ci_ind] at universe
     instance [u] and parameters [params]. If this match has case
     inversion (ie match on a 1 constructor SProp inductive with
     proof relevant return type) the indices are in [iv].

     The names in [p] are the names of the bound indices and
     inductive value (ie the [in] and [as] clauses).

     The names in the [brs] are the names of the variables bound in the respective branch. *)
  | Fix       of ('constr, 'types, 'r) pfixpoint
  | CoFix     of ('constr, 'types, 'r) pcofixpoint
  | Proj      of Projection.t * 'r * 'constr
  (** The relevance is the relevance of the whole term *)
  | Int       of Uint63.t
  | Float     of Float64.t
  | String    of Pstring.t
  | Array     of 'univs * 'constr array * 'constr * 'types
  (** [Array (u,vals,def,t)] is an array of [vals] in type [t] with default value [def].
      [u] is a universe containing [t]. *)

(** User view of [constr]. For [App], it is ensured there is at
   least one argument and the function is not itself an applicative
   term *)

val kind : constr -> (constr, types, Sorts.t, UVars.Instance.t, Sorts.relevance) kind_of_term
val of_kind : (constr, types, Sorts.t, UVars.Instance.t, Sorts.relevance) kind_of_term -> constr

val kind_nocast_gen : ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term) ->
  ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term)

val kind_nocast : constr -> (constr, types, Sorts.t, UVars.Instance.t, Sorts.relevance) kind_of_term

(** {6 Simple case analysis} *)
val isRel  : constr -> bool
val isRelN : int -> constr -> bool
val isVar  : constr -> bool
val isVarId : Id.t -> constr -> bool
val isRef : constr -> bool
val isRefX : GlobRef.t -> constr -> bool
val isInd  : constr -> bool
val isEvar : constr -> bool
val isMeta : constr -> bool
val isEvar_or_Meta : constr -> bool
val isSort : constr -> bool
val isCast : constr -> bool
val isApp : constr -> bool
val isLambda : constr -> bool
val isLetIn : constr -> bool
val isProd : constr -> bool
val isConst : constr -> bool
val isConstruct : constr -> bool
val isFix : constr -> bool
val isCoFix : constr -> bool
val isCase : constr -> bool
val isProj : constr -> bool

val is_Prop : constr -> bool
val is_Set  : constr -> bool
val isprop : constr -> bool
val is_Type : constr -> bool
val iskind : constr -> bool
val is_small : Sorts.t -> bool

(** {6 Term destructors } *)
(** Destructor operations are partial functions and
    @raise DestKO if the term has not the expected form. *)

exception DestKO

(** Destructs a de Bruijn index *)
val destRel : constr -> int

(** Destructs an existential variable *)
val destMeta : constr -> metavariable

(** Destructs a variable *)
val destVar : constr -> Id.t

(** Destructs a sort. [is_Prop] recognizes the sort [Prop], whether
   [isprop] recognizes both [Prop] and [Set]. *)
val destSort : constr -> Sorts.t

(** Destructs a casted term *)
val destCast : constr -> constr * cast_kind * constr

(** Destructs the product {% $ %}(x:t_1)t_2{% $ %} *)
val destProd : types -> Name.t binder_annot * types * types

(** Destructs the abstraction {% $ %}[x:t_1]t_2{% $ %} *)
val destLambda : constr -> Name.t binder_annot * types * constr

(** Destructs the let {% $ %}[x:=b:t_1]t_2{% $ %} *)
val destLetIn : constr -> Name.t binder_annot * constr * types * constr

(** Destructs an application *)
val destApp : constr -> constr * constr array

(** Decompose any term as an applicative term; the list of args can be empty *)
val decompose_app_list : constr -> constr * constr list

(** Same as [decompose_app], but returns an array. *)
val decompose_app : constr -> constr * constr array

(** Destructs a constant *)
val destConst : constr -> Constant.t UVars.puniverses

(** Destructs an existential variable *)
val destEvar : constr -> existential

(** Destructs a (co)inductive type *)
val destInd : constr -> inductive UVars.puniverses

(** Destructs a constructor *)
val destConstruct : constr -> constructor UVars.puniverses

(** Destructs a [match c as x in I args return P with ... |
Ci(...yij...) => ti | ... end] (or [let (..y1i..) := c as x in I args
return P in t1], or [if c then t1 else t2])
@return [(info,c,fun args x => P,[|...|fun yij => ti| ...|])]
where [info] is pretty-printing information *)
val destCase : constr -> case

(** Destructs a projection *)
val destProj : constr -> Projection.t * Sorts.relevance * constr

(** Destructs the {% $ %}i{% $ %}th function of the block
   [Fixpoint f{_ 1} ctx{_ 1} = b{_ 1}
    with    f{_ 2} ctx{_ 2} = b{_ 2}
    ...
    with    f{_ n} ctx{_ n} = b{_ n}],
   where the length of the {% $ %}j{% $ %}th context is {% $ %}ij{% $ %}.
*)
val destFix : constr -> fixpoint

val destCoFix : constr -> cofixpoint

val destRef : constr -> GlobRef.t UVars.puniverses

(** {6 Equality} *)

(** [equal a b] is true if [a] equals [b] modulo alpha, casts,
   and application grouping *)
val equal : constr -> constr -> bool

(** [eq_constr_univs u a b] is [true] if [a] equals [b] modulo alpha, casts,
   application grouping and the universe equalities in [u]. *)
val eq_constr_univs : constr UGraph.check_function

(** [leq_constr_univs u a b] is [true] if [a] is convertible to [b] modulo
    alpha, casts, application grouping and the universe inequalities in [u]. *)
val leq_constr_univs : constr UGraph.check_function

(** [eq_constr_univs a b] [true, c] if [a] equals [b] modulo alpha, casts,
   application grouping and ignoring universe instances. *)
val eq_constr_nounivs : constr -> constr -> bool

(** Total ordering compatible with [equal] *)
val compare : constr -> constr -> int

(** {6 Extension of Context with declarations on constr} *)

type rel_declaration = (constr, types, Sorts.relevance) Context.Rel.Declaration.pt
type named_declaration = (constr, types, Sorts.relevance) Context.Named.Declaration.pt
type compacted_declaration = (constr, types, Sorts.relevance) Context.Compacted.Declaration.pt
type rel_context = rel_declaration list
type named_context = named_declaration list
type compacted_context = compacted_declaration list

(** {6 Relocation and substitution } *)

(** [exliftn el c] lifts [c] with lifting [el] *)
val exliftn : Esubst.lift -> constr -> constr

(** [liftn n k c] lifts by [n] indexes above or equal to [k] in [c] *)
val liftn : int -> int -> constr -> constr

(** [lift n c] lifts by [n] the positive indexes in [c] *)
val lift : int -> constr -> constr

(** {6 Functionals working on expressions canonically abstracted over
       a local context (possibly with let-ins)} *)

(** [map_branches f br] maps [f] on the immediate subterms of an array
   of "match" branches [br] in canonical eta-let-expanded form; it is
   not recursive and the order with which subterms are processed is
   not specified; it preserves sharing; the immediate subterms are the
   types and possibly terms occurring in the context of each branch as
   well as the body of each branch *)

val map_branches : (constr -> constr) -> case_branch array -> case_branch array

(** [map_return_predicate f p] maps [f] on the immediate subterms of a
   return predicate of a "match" in canonical eta-let-expanded form;
   it is not recursive and the order with which subterms are processed
   is not specified; it preserves sharing; the immediate subterms are
   the types and possibly terms occurring in the context of each
   branch as well as the body of the predicate *)

val map_return_predicate : (constr -> constr) -> case_return -> case_return

(** [map_branches_with_binders f br] maps [f] on the immediate
   subterms of an array of "match" branches [br] in canonical
   eta-let-expanded form; it carries an extra data [n] (typically a
   lift index) which is processed by [g] (which typically adds 1 to
   [n]) at each binder traversal; it is not recursive and the order
   with which subterms are processed is not specified; it preserves
   sharing; the immediate subterms are the types and possibly terms
   occurring in the context of the branch as well as the body of the
   branch *)

val map_branches_with_binders : ('a -> 'a) -> ('a -> constr -> constr) -> 'a -> case_branch array -> case_branch array

(** [map_return_predicate_with_binders f p] maps [f] on the immediate
   subterms of a return predicate of a "match" in canonical
   eta-let-expanded form; it carries an extra data [n] (typically a
   lift index) which is processed by [g] (which typically adds 1 to
   [n]) at each binder traversal; it is not recursive and the order
   with which subterms are processed is not specified; it preserves
   sharing; the immediate subterms are the types and possibly terms
   occurring in the context of each branch as well as the body of the
   predicate *)

val map_return_predicate_with_binders : ('a -> 'a) -> ('a -> constr -> constr) -> 'a -> case_return -> case_return

(** {6 Functionals working on the immediate subterm of a construction } *)

(** [fold f acc c] folds [f] on the immediate subterms of [c]
   starting from [acc] and proceeding from left to right according to
   the usual representation of the constructions; it is not recursive *)

val fold : ('a -> constr -> 'a) -> 'a -> constr -> 'a

val fold_invert : ('a -> 'b -> 'a) -> 'a -> 'b pcase_invert -> 'a

(** [map f c] maps [f] on the immediate subterms of [c]; it is
   not recursive and the order with which subterms are processed is
   not specified *)

val map : (constr -> constr) -> constr -> constr

val map_invert : ('a -> 'a) -> 'a pcase_invert -> 'a pcase_invert

(** Like {!map}, but also has an additional accumulator. *)

val fold_map : ('a -> constr -> 'a * constr) -> 'a -> constr -> 'a * constr

val fold_map_invert : ('a -> 'b -> 'a * 'b) ->
  'a -> 'b pcase_invert -> 'a * 'b pcase_invert

(** [map_with_binders g f n c] maps [f n] on the immediate
   subterms of [c]; it carries an extra data [n] (typically a lift
   index) which is processed by [g] (which typically add 1 to [n]) at
   each binder traversal; it is not recursive and the order with which
   subterms are processed is not specified *)

val map_with_binders :
  ('a -> 'a) -> ('a -> constr -> constr) -> 'a -> constr -> constr

(** [iter f c] iters [f] on the immediate subterms of [c]; it is
   not recursive and the order with which subterms are processed is
   not specified *)

val iter : (constr -> unit) -> constr -> unit

val iter_invert : ('a -> unit) -> 'a pcase_invert -> unit

(** [iter_with_binders g f n c] iters [f n] on the immediate
   subterms of [c]; it carries an extra data [n] (typically a lift
   index) which is processed by [g] (which typically add 1 to [n]) at
   each binder traversal; it is not recursive and the order with which
   subterms are processed is not specified *)

val iter_with_binders :
  ('a -> 'a) -> ('a -> constr -> unit) -> 'a -> constr -> unit

(** [iter_with_binders g f n c] iters [f n] on the immediate
   subterms of [c]; it carries an extra data [n] (typically a lift
   index) which is processed by [g] (which typically add 1 to [n]) at
   each binder traversal; it is not recursive and the order with which
   subterms are processed is not specified *)

val fold_constr_with_binders :
  ('a -> 'a) -> ('a -> 'b -> constr -> 'b) -> 'a -> 'b -> constr -> 'b

type 'constr constr_compare_fn = int -> 'constr -> 'constr -> bool

(** [compare_head f c1 c2] compare [c1] and [c2] using [f] to compare
   the immediate subterms of [c1] of [c2] if needed; Cast's, binders
   name and Cases annotations are not taken into account *)

val compare_head : (existential -> existential -> bool) -> constr constr_compare_fn -> constr constr_compare_fn

(** Convert a global reference applied to 2 instances. The int says
   how many arguments are given (as we can only use cumulativity for
   fully applied inductives/constructors) .*)
type 'univs instance_compare_fn = (GlobRef.t * int) option ->
  'univs -> 'univs -> bool

(** [compare_head_gen u s f c1 c2] compare [c1] and [c2] using [f] to
   compare the immediate subterms of [c1] of [c2] if needed, [u] to
   compare universe instances, [s] to compare sorts; Cast's, binders
   name and Cases annotations are not taken into account *)

val compare_head_gen : UVars.Instance.t instance_compare_fn ->
  (Sorts.t -> Sorts.t -> bool) ->
  (existential -> existential -> bool) ->
  constr constr_compare_fn ->
  constr constr_compare_fn

val compare_head_gen_leq_with :
  ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term) ->
  ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term) ->
  'univs instance_compare_fn ->
  ('sort -> 'sort -> bool) ->
  ('v pexistential -> 'v pexistential -> bool) ->
  'v constr_compare_fn ->
  'v constr_compare_fn ->
  'v constr_compare_fn

(** [compare_head_gen_with k1 k2 u s f c1 c2] compares [c1] and [c2]
    like [compare_head_gen u s f c1 c2], except that [k1] (resp. [k2])
    is used,rather than {!kind}, to expose the immediate subterms of
    [c1] (resp. [c2]). *)
val compare_head_gen_with :
  ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term) ->
  ('v -> ('v, 'v, 'sort, 'univs, 'r) kind_of_term) ->
  'univs instance_compare_fn ->
  ('sort -> 'sort -> bool) ->
  ('v pexistential -> 'v pexistential -> bool) ->
  'v constr_compare_fn ->
  'v constr_compare_fn

(** [compare_head_gen_leq u s f fle c1 c2] compare [c1] and [c2] using
    [f] to compare the immediate subterms of [c1] of [c2] for
    conversion, [fle] for cumulativity, [u] to compare universe
    instances (the first boolean tells if they belong to a Constant.t),
    [s] to compare sorts for for subtyping; Cast's, binders name and
    Cases annotations are not taken into account *)

val compare_head_gen_leq : UVars.Instance.t instance_compare_fn ->
  (Sorts.t -> Sorts.t -> bool) ->
  (existential -> existential -> bool) ->
  constr constr_compare_fn ->
  constr constr_compare_fn ->
  constr constr_compare_fn

val eq_invert : ('a -> 'a -> bool)
  -> 'a pcase_invert -> 'a pcase_invert -> bool

(** {6 Hashconsing} *)

val hash : constr -> int

(*********************************************************************)

module GenHCons(C:sig
    type t
    val kind : t -> (t, t, Sorts.t, UVars.Instance.t, Sorts.relevance) kind_of_term
    val self : t -> constr
    val refcount : t -> int

    val via_hconstr : bool

    module Tbl : sig
      val find_opt : t -> (int * constr) option
      val add : t -> int * constr -> unit
    end
  end) : sig
  val hcons : C.t -> int * constr
end


val hcons : constr Hashcons.f

val hasheq_kind : (_ kind_of_term as 'k) -> 'k -> bool
(** Checks physical equality of every immediate element (goes inside tuples and arrays) *)

val debug_print : constr -> Pp.t
val debug_print_fix : ('a -> Pp.t) -> ('a, 'a, 'r) pfixpoint -> Pp.t

val mkConst : Constant.t -> constr
[@@deprecated "(8.18) Use [mkConstU] or if truly needed [UnsafeMonomorphic.mkConst]"]

val mkInd : inductive -> constr
[@@deprecated "(8.18) Use [mkIndU] or if truly needed [UnsafeMonomorphic.mkInd]"]

val mkConstruct : constructor -> constr
[@@deprecated "(8.18) Use [mkConstructU] or if truly needed [UnsafeMonomorphic.mkConstruct]"]

val hcons_annot : Name.t binder_annot Hashcons.f

val hcons_caseinfo : case_info Hashcons.f

val hash_cast_kind : cast_kind -> int
