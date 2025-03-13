(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open Util
open Univ
open PolyConstraints

module Variance =
struct
  (** A universe position in the instance given to a cumulative
     inductive can be the following. Note there is no Contravariant
     case because [forall x : A, B <= forall x : A', B'] requires [A =
     A'] as opposed to [A' <= A]. *)
  type t = Irrelevant | Covariant | Invariant

  let sup x y =
    match x, y with
    | Irrelevant, s | s, Irrelevant -> s
    | Invariant, _ | _, Invariant -> Invariant
    | Covariant, Covariant -> Covariant

  let equal a b = match a,b with
    | Irrelevant, Irrelevant | Covariant, Covariant | Invariant, Invariant -> true
    | (Irrelevant | Covariant | Invariant), _ -> false

  let check_subtype x y = match x, y with
  | (Irrelevant | Covariant | Invariant), Irrelevant -> true
  | Irrelevant, Covariant -> false
  | (Covariant | Invariant), Covariant -> true
  | (Irrelevant | Covariant), Invariant -> false
  | Invariant, Invariant -> true

  let pr = function
    | Irrelevant -> str "*"
    | Covariant -> str "+"
    | Invariant -> str "="

  let leq_constraint csts variance u u' =
    match variance with
    | Irrelevant -> csts
    | Covariant -> enforce_leq_level u u' csts
    | Invariant -> enforce_eq_level u u' csts

  let eq_constraint csts variance u u' =
    match variance with
    | Irrelevant -> csts
    | Covariant | Invariant -> enforce_eq_level u u' csts

  let leq_constraints variance u u' csts =
    let len = Array.length u in
    assert (len = Array.length u' && len = Array.length variance);
    Array.fold_left3 leq_constraint csts variance u u'

  let eq_constraints variance u u' csts =
    let len = Array.length u in
    assert (len = Array.length u' && len = Array.length variance);
    Array.fold_left3 eq_constraint csts variance u u'
end

module Instance =
struct
  type t = Quality.t array * Level.t array

  let empty : t = [||], [||]

  module HInstancestruct =
  struct
    type nonrec t = t
    type u = (Quality.t -> Quality.t) * (Level.t -> Level.t)

    let hashcons (hqual, huniv) (aq, au as a) =
      let qlen = Array.length aq in
      let ulen = Array.length au in
        if Int.equal qlen 0 && Int.equal ulen 0 then empty
        else begin
          for i = 0 to qlen - 1 do
            let x = Array.unsafe_get aq i in
            let x' = hqual x in
              if x == x' then ()
              else Array.unsafe_set aq i x'
          done;
          for i = 0 to ulen - 1 do
            let x = Array.unsafe_get au i in
            let x' = huniv x in
              if x == x' then ()
              else Array.unsafe_set au i x'
          done;
          a
        end

    let eq t1 t2 =
      CArray.equal (==) (fst t1) (fst t2)
      && CArray.equal (==) (snd t1) (snd t2)

    let hash (aq,au) =
      let accu = ref 0 in
        for i = 0 to Array.length aq - 1 do
          let l = Array.unsafe_get aq i in
          let h = Quality.hash l in
            accu := Hashset.Combine.combine !accu h;
        done;
        for i = 0 to Array.length au - 1 do
          let l = Array.unsafe_get au i in
          let h = Level.hash l in
            accu := Hashset.Combine.combine !accu h;
        done;
        (* [h] must be positive. *)
        let h = !accu land 0x3FFFFFFF in
          h
  end

  module HInstance = Hashcons.Make(HInstancestruct)

  let hcons = Hashcons.simple_hcons HInstance.generate HInstance.hcons (Quality.hcons,Level.hcons)

  let hash = HInstancestruct.hash

  let share a = (hcons a, hash a)

  let empty = hcons empty

  let is_empty (x,y) = CArray.is_empty x && CArray.is_empty y


  let append (xq,xu as x) (yq,yu as y) =
    if is_empty x then y
    else if is_empty y then x
    else Array.append xq yq, Array.append xu yu

  let of_array a : t = a

  let to_array (a:t) = a

  let abstract_instance (qlen,ulen) =
    let qs = Array.init qlen Quality.var in
    let us = Array.init ulen Level.var in
    of_array (qs,us)

  let length (aq,au) = Array.length aq, Array.length au

  let subst_fn (fq, fn) (q,u as orig) : t =
    let q' = CArray.Smart.map (Quality.subst fq) q in
    let u' = CArray.Smart.map fn u in
    if q' == q && u' == u then orig else q', u'

  let levels (xq,xu) =
    let q = Array.fold_left (fun acc x -> Quality.Set.add x acc) Quality.Set.empty xq in
    let u = Array.fold_left (fun acc x -> Level.Set.add x acc) Level.Set.empty xu in
    q, u

  let pr_qualities prq (q,_) =
    if Array.is_empty q
    then mt()
    else prvect_with_sep spc (Quality.pr prq) q

  let pr_levels prl ?variance (_,u) =
    let ppu i u =
      let v = Option.map (fun v -> v.(i)) variance in
      pr_opt_no_spc Variance.pr v ++ prl u
    in
    prvecti_with_sep spc ppu u

  let pr prq prl ?variance (q,u) =
    let sep = strbrk @@ if Array.is_empty q then "" else " | " in
    pr_qualities prq (q,u) ++ sep
    ++ pr_levels prl ?variance (q,u)

  let equal (xq,xu) (yq,yu) =
    CArray.equal Quality.equal xq yq
    && CArray.equal Level.equal xu yu

  type mask = Quality.pattern array * int option array

  let pattern_match (qmask, umask) (qs, us) tqus =
    let tqus = Array.fold_left2 (fun tqus mask u -> Partial_subst.maybe_add_univ mask u tqus) tqus umask us in
    match Array.fold_left2 (fun tqus mask q -> Quality.pattern_match mask q tqus |> function Some tqs -> tqs | None -> raise_notrace Exit) tqus qmask qs with
    | tqs -> Some tqs
    | exception Exit -> None
end

let eq_sizes (a,b) (a',b') = Int.equal a a' && Int.equal b b'

(* type 'a quconstraint_function = 'a -> 'a -> PolyConstraints.t -> PolyConstraints.t *)

let enforce_eq_instances x y csts =
  let xq, xu = Instance.to_array x and yq, yu = Instance.to_array y in
  if Array.length xq != Array.length yq || Array.length xu != Array.length yu then
    CErrors.anomaly (Pp.(++) (Pp.str "Invalid argument: enforce_eq_instances called with")
                       (Pp.str " instances of different lengths."));
  let csts' = CArray.fold_right2 enforce_eq_quality xq yq csts in
  let csts' = CArray.fold_right2 enforce_eq_level xu yu csts' in
  if csts' == csts then csts else csts'

let enforce_eq_variance_instances variances x y csts =
  let xq, xu = Instance.to_array x and yq, yu = Instance.to_array y in
  let csts' = CArray.fold_right2 enforce_eq_quality xq yq csts in
  let csts' = Variance.eq_constraints variances xu yu csts' in
  if csts' == csts then csts else csts'

let enforce_leq_variance_instances variances x y csts =
  let xq, xu = Instance.to_array x and yq, yu = Instance.to_array y in
  (* no variance for quality variables -> enforce_eq *)
  let csts' = CArray.fold_right2 enforce_eq_quality xq yq csts in
  let csts' = Variance.leq_constraints variances xu yu csts' in
  if csts' == csts then csts else csts'

let subst_instance_level s l =
  match Level.var_index l with
  | Some n -> (snd (Instance.to_array s)).(n)
  | None -> l

let subst_instance_qvar s v =
  match Quality.QVar.var_index v with
  | Some n -> (fst (Instance.to_array s)).(n)
  | None -> Quality.QVar v

let subst_instance_quality s l =
  match l with
  | Quality.QVar v -> begin match Quality.QVar.var_index v with
      | Some n -> (fst (Instance.to_array s)).(n)
      | None -> l
    end
  | Quality.QConstant _ -> l

let subst_instance_instance s i =
  let qs, us = Instance.to_array i in
  let qs' = Array.Smart.map (fun l -> subst_instance_quality s l) qs in
  let us' = Array.Smart.map (fun l -> subst_instance_level s l) us in
  if qs' == qs && us' == us then i else Instance.of_array (qs', us')

let subst_instance_universe s univ =
  let f (v,n as vn) =
    let v' = subst_instance_level s v in
    if v == v' then vn
    else v', n
  in
  let u = Universe.repr univ in
  let u' = List.Smart.map f u in
  if u == u' then univ
  else Universe.unrepr u'

let subst_instance_sort u s =
  Sorts.subst_fn ((subst_instance_qvar u), (subst_instance_universe u)) s

let subst_instance_relevance u r =
  Sorts.relevance_subst_fn (subst_instance_qvar u) r

let subst_instance_constraint subst_instance s (u, d, v as c) =
  let u' = subst_instance s u in
  let v' = subst_instance s v in
    if u' == u && v' == v then c
    else (u', d, v')

let subst_instance_elim_constraint =
  subst_instance_constraint subst_instance_quality

let subst_instance_level_constraint =
  subst_instance_constraint subst_instance_level

let subst_instance_constraints s csts =
  let open Quality in
  PolyConstraints.fold
    ( (fun q csts -> ElimConstraints.add (subst_instance_elim_constraint s q) csts)
    , (fun l csts -> LvlConstraints.add (subst_instance_level_constraint s l) csts))
    csts PolyConstraints.empty

type 'a puniverses = 'a * Instance.t
let out_punivs (x, _y) = x
let in_punivs x = (x, Instance.empty)
let eq_puniverses f (x, u) (y, u') =
  f x y && Instance.equal u u'

type bound_names =
  { qualities : Names.Name.t array
  ; levels : Names.Name.t array }

(** A context of universe levels with universe constraints,
    representing local universe variables and constraints *)

module PolyContext =
struct
  type t = bound_names * Instance.t constrained

  let make names (univs, _ as x) : t =
    let qs, us = Instance.to_array univs in
    assert (Array.length (names.qualities) = Array.length qs &&
	      Array.length(names.levels) = Array.length us);
    (names, x)

  (** Universe contexts (variables as a list) *)
  let empty =
    ({ qualities = [| |]; levels = [| |] }, (Instance.empty, PolyConstraints.empty))
  let is_empty (_, (univs, csts)) =
    Instance.is_empty univs && PolyConstraints.is_empty csts

  let pr prq prl ?variance (_, (univs, csts) as uctx) =
    if is_empty uctx then mt() else
      h (Instance.pr prq prl ?variance univs ++ str " |= ") ++
	h (v 0 (PolyConstraints.pr prq prl csts))

  let hcons (names, (levels, csts)) =
    ( { qualities = Array.map Names.Name.hcons names.qualities
      ; levels = Array.map Names.Name.hcons names.levels }
    , ( Instance.hcons levels
      , hcons_poly_constraints csts))

  let names ((names, _) : t) = names
  let instance (_, (univs, _csts)) = univs
  let constraints (_, (_univs, csts)) = csts

  let union (names, (univs, csts)) (names', (univs', csts')) =
    ( { qualities = Array.append names.qualities names'.qualities
      ; levels = Array.append names.levels names'.levels }
    , ( Instance.append univs univs'
      , PolyConstraints.union csts csts'))

  let size (_,(x,_)) = Instance.length x

  let refine_names names (names', x) =
    let merge_names =
      Array.map2 Names.(fun old refined -> match refined with Anonymous -> old | Name _ -> refined) in
    ( { qualities = merge_names names.qualities names'.qualities
      ; levels = merge_names names.levels names'.levels }
    , x)

  let sort_levels a =
    Array.sort Level.compare a; a

  let sort_qualities a =
    Array.sort Quality.compare a; a

  let of_context_set f qctx (levels, csts) =
    let qctx = sort_qualities
        (Array.map_of_list (fun q -> Quality.QVar q)
           (Quality.QVar.Set.elements qctx))
    in
    let levels = sort_levels (Array.of_list (Level.Set.elements levels)) in
    let inst = Instance.of_array (qctx, levels) in
    (f inst, (inst, csts))

  let to_context_set (_, (inst, csts)) =
    let qs, us = Instance.to_array inst in
    let us = Array.fold_left (fun acc x -> Level.Set.add x acc) Level.Set.empty us in
    let qs = Array.fold_left (fun acc -> function
        | Quality.QVar x -> Quality.QVar.Set.add x acc
        | Quality.QConstant _ -> assert false)
        Quality.QVar.Set.empty
        qs
    in
    qs, (us, csts)

end

type poly_context = PolyContext.t
type 'a in_poly_context = 'a * poly_context

let hcons_poly_context = PolyContext.hcons

module AbstractContext =
struct
  type t = bound_names constrained

  let make names csts : t = names, csts

  let instantiate inst (names, cst) =
    let q, u = Instance.to_array inst in
    assert (Array.length q == Array.length names.qualities
	    && Array.length u = Array.length names.levels);
    subst_instance_constraints inst cst

  let names (nas, _) = nas

  let hcons (names, csts) =
    ( { qualities = Array.map Names.Name.hcons names.qualities
      ; levels = Array.map Names.Name.hcons names.levels }
    , hcons_poly_constraints csts)

  let empty = ({ qualities = [| |]; levels = [| |] }, PolyConstraints.empty)

  let is_constant (names, _) =
    Array.is_empty names.qualities && Array.is_empty names.levels

  let is_empty (_, csts as ctx) =
    is_constant ctx && PolyConstraints.is_empty csts

  let union (names, csts) (names', csts') =
    ( { qualities = Array.append names.qualities names'.qualities
      ; levels = Array.append names.levels names'.levels }
    , PolyConstraints.union csts csts')

  let size (names, _) = Array.length names.qualities, Array.length names.levels

  let repr (names, csts as ctx) : PolyContext.t =
    let inst = Instance.abstract_instance (size ctx) in
    (names, (inst, csts))

  let pr prq pru ?variance ctx = PolyContext.pr prq pru ?variance (repr ctx)

end

type 'a univ_abstracted = {
  univ_abstracted_value : 'a;
  univ_abstracted_binder : AbstractContext.t;
}

let map_univ_abstracted f {univ_abstracted_value;univ_abstracted_binder} =
  let univ_abstracted_value = f univ_abstracted_value in
  {univ_abstracted_value;univ_abstracted_binder}

let hcons_abstract_universe_context = AbstractContext.hcons

(** Substitutions. *)

(** A universe level substitution, note that no algebraic universes are
    involved *)

type universe_level_subst = Univ.Level.t Level.Map.t

(** A set of universes with universe constraints.
    We linearize the set to a list after typechecking.
    Beware, representation could change.
*)

let empty_level_subst = Level.Map.empty
let is_empty_level_subst = Level.Map.is_empty

(** Substitution functions *)

(** With level to level substitutions. *)
let subst_univs_level_level subst l =
  try Level.Map.find l subst
  with Not_found -> l

let subst_univs_level_universe subst =
  Universe.map (fun u -> subst_univs_level_level subst u)

let subst_univs_level_constraint subst (u,d,v) =
  let u' = subst_univs_level_level subst u
  and v' = subst_univs_level_level subst v in
    if d != Lt && Level.equal u' v' then None
    else Some (u',d,v')

let subst_univs_level_constraints subst csts =
  PolyConstraints.fold
    ( (fun _ qc -> qc)
    , (fun c -> Option.fold_right LvlConstraints.add (subst_univs_level_constraint subst c)))
    csts PolyConstraints.empty

let pr_universe_level_subst prl =
  Level.Map.pr prl (fun u -> str" := " ++ prl u ++ spc ())


let pr_quality_level_subst prl l =
  let open Pp in
  h (prlist_with_sep fnl (fun (u,v) -> prl u ++ str " := " ++ Quality.pr prl v)
       (Quality.QVar.Map.bindings l))

type sort_level_subst = Quality.t Quality.QVar.Map.t * universe_level_subst

let is_empty_sort_subst (qsubst,usubst) = Quality.QVar.Map.is_empty qsubst && is_empty_level_subst usubst

let empty_sort_subst = Quality.QVar.Map.empty, empty_level_subst

let subst_sort_level_instance (qsubst,usubst) i =
  let i' = Instance.subst_fn (Quality.subst_fn qsubst, subst_univs_level_level usubst) i in
  if i == i' then i
  else i'

let subst_instance_sort_level_subst s (i : sort_level_subst) =
  let qs, us = i in
  let qs' = Quality.QVar.Map.map (fun l -> subst_instance_quality s l) qs in
  let us' = Level.Map.map (fun l -> subst_instance_level s l) us in
  if qs' == qs && us' == us then i else (qs', us')

let subst_univs_level_abstract_universe_context subst (inst, csts) =
  inst, subst_univs_level_constraints subst csts

let subst_sort_level_qvar (qsubst,_) qv =
  match Quality.QVar.Map.find_opt qv qsubst with
  | None -> Quality.QVar qv
  | Some q -> q

let subst_sort_level_quality subst = function
  | Quality.QConstant _ as q -> q
  | Quality.QVar q ->
    subst_sort_level_qvar subst q

let subst_sort_level_sort (_,usubst as subst) s =
  let fq qv = subst_sort_level_qvar subst qv in
  let fu u = subst_univs_level_universe usubst u in
  Sorts.subst_fn (fq,fu) s

let subst_sort_level_relevance subst r =
  Sorts.relevance_subst_fn (subst_sort_level_qvar subst) r

let make_instance_subst i =
  let qarr, uarr = Instance.to_array i in
  let qsubst =
    Array.fold_left_i (fun i acc l ->
      let l = match l with Quality.QVar l -> l | _ -> assert false in
      Quality.QVar.Map.add l (Quality.var i) acc)
      Quality.QVar.Map.empty qarr
  in
  let usubst =
    Array.fold_left_i (fun i acc l ->
      Level.Map.add l (Level.var i) acc)
      Level.Map.empty uarr
  in
  qsubst, usubst

let make_abstract_instance ctx =
  PolyContext.instance (AbstractContext.repr ctx)

let abstract_universes uctx =
  let nas = PolyContext.names uctx in
  let instance = PolyContext.instance uctx in
  let subst = make_instance_subst instance in
  let cstrs = subst_univs_level_constraints (snd subst)
      (PolyContext.constraints uctx)
  in
  let ctx = (nas, cstrs) in
  instance, ctx

let pr_poly_context = PolyContext.pr

let pr_abstract_context = AbstractContext.pr
