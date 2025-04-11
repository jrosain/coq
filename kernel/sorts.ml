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

type t =
  | SProp
  | Prop
  | Set
  | Ghost of Universe.t
  | Type of Universe.t
  | QSort of QVar.t * Universe.t

let sprop = SProp
let prop = Prop
let set = Set
let type1 = Type Universe.type1
let qsort q u = QSort (q, u)
let ghost u = Ghost u

let sort_of_univ u =
  if Universe.is_type0 u then set else Type u

let univ_of_sort s =
  match s with
  | SProp | Prop | Set -> Universe.type0
  | Type u | Ghost u | QSort (_, u) -> u

let make q u =
  let open Quality in
  match q with
  | QVar q -> qsort q u
  | QConstant QSProp -> sprop
  | QConstant QProp -> prop
  | QConstant QType -> sort_of_univ u
  | QConstant QGhost -> ghost u

let compare s1 s2 =
  if s1 == s2 then 0 else
    match s1, s2 with
    | SProp, SProp -> 0
    | SProp, (Prop | Set | Type _ | Ghost _ | QSort _) -> -1
    | (Prop | Set | Type _ | Ghost _ | QSort _), SProp -> 1
    | Prop, Prop -> 0
    | Prop, (Set | Type _ | Ghost _ | QSort _) -> -1
    | Set, Prop -> 1
    | Set, Set -> 0
    | Set, (Type _ | Ghost _ | QSort _) -> -1
    | Type _, QSort _ -> -1
    | Type u1, Type u2 -> Universe.compare u1 u2
    | Type _, (Prop | Set | Ghost _) -> 1
    | QSort (q1, u1), QSort (q2, u2) ->
      let c = QVar.compare q1 q2 in
      if Int.equal c 0 then Universe.compare u1 u2 else c
    | QSort _, (Prop | Set | Type _ | Ghost _) -> 1
    | Ghost u1, Ghost u2 -> Universe.compare u1 u2
    | Ghost _, QSort _ -> -1
    | Ghost _, (Prop | Set | Type _) -> 1

let equal s1 s2 = Int.equal (compare s1 s2) 0

let super = function
  | SProp | Prop | Set -> Type (Universe.type1)
  | Type u | QSort (_, u) -> Type (Universe.super u)
  | Ghost u -> Ghost (Universe.super u)

let is_sprop = function
  | SProp -> true
  | Prop | Set | Type _ | QSort _ | Ghost _ -> false

let is_prop = function
  | Prop -> true
  | SProp | Set | Type _ | QSort _ | Ghost _ -> false

let is_set = function
  | Set -> true
  | SProp | Prop | Type _ | QSort _ | Ghost _ -> false

let is_small = function
  | SProp | Prop | Set -> true
  | Ghost _ | Type _ | QSort _ -> false

let levels s = match s with
| SProp | Prop -> Level.Set.empty
| Set -> Level.Set.singleton Level.set
| Type u | Ghost u | QSort (_, u) -> Universe.levels u

let subst_fn (fq,fu) = function
  | SProp | Prop | Set as s -> s
  | Type v as s ->
    let v' = fu v in
    if v' == v then s else sort_of_univ v'
  | Ghost v as s ->
    let v' = fu v in
    if v' == v then s else ghost v'
  | QSort (q, v) as s ->
    let open Quality in
    match fq q with
    | QVar q' ->
      let v' = fu v in
      if q' == q && v' == v then s
      else qsort q' v'
    | QConstant QSProp -> sprop
    | QConstant QProp -> prop
    | QConstant QGhost -> ghost (fu v)
    | QConstant QType -> sort_of_univ (fu v)

let quality = let open Quality in function
| Set | Type _ -> qtype
| Ghost _ -> qghost
| Prop -> qprop
| SProp -> qsprop
| QSort (q, _) -> QVar q

open Hashset.Combine

let hash = function
  | SProp -> combinesmall 1 0
  | Prop -> combinesmall 1 1
  | Set -> combinesmall 1 2
  | Type u ->
    let h = Univ.Universe.hash u in
    combinesmall 2 h
  | QSort (q, u) ->
    let h = Univ.Universe.hash u in
    let h' = QVar.hash q in
    combinesmall 3 (combine h h')
  | Ghost u ->
    let h = Univ.Universe.hash u in
    combinesmall 4 h

module HSorts =
  Hashcons.Make(
    struct
      type nonrec t = t

      let hashcons = function
        | Type u as c ->
          let hu, u' = Universe.hcons u in
          combinesmall 2 hu, if u' == u then c else Type u'
        | QSort (q, u) as c ->
          let hq, q' = QVar.hcons q in
          let hu, u' = Universe.hcons u in
          combinesmall 3 (combine hu hq), if u' == u && q' == q then c else QSort (q', u')
        | Ghost u as c ->
          let hu, u' = Universe.hcons u in
          combinesmall 4 hu, if u' == u then c else Ghost u'
        | SProp | Prop | Set as s -> hash s, s
      let eq s1 s2 = match (s1,s2) with
        | SProp, SProp | Prop, Prop | Set, Set -> true
        | Type u1, Type u2 | Ghost u1, Ghost u2 -> u1 == u2
        | QSort (q1, u1), QSort (q2, u2) -> q1 == q2 && u1 == u2
        | (SProp | Prop | Set | Ghost _ | Type _ | QSort _), _ -> false
    end)

let hcons = Hashcons.simple_hcons HSorts.generate HSorts.hcons ()

(** On binders: is this variable proof relevant *)
type relevance = Relevant | CIrrelevant | Irrelevant | RelevanceVar of QVar.t

let relevance_equal r1 r2 = match r1,r2 with
  | Relevant, Relevant | Irrelevant, Irrelevant | CIrrelevant, CIrrelevant -> true
  | RelevanceVar q1, RelevanceVar q2 -> QVar.equal q1 q2
  | (Relevant | Irrelevant | CIrrelevant | RelevanceVar _), _ -> false

let relevance_hash = function
  | Relevant -> 0
  | Irrelevant -> 1
  | CIrrelevant -> 2
  | RelevanceVar q -> Hashset.Combine.combinesmall 3 (QVar.hash q)

let relevance_subst_fn f = function
  | Relevant | Irrelevant | CIrrelevant as r -> r
  | RelevanceVar qv as r ->
    let open Quality in
    match f qv with
    | QConstant (QSProp) -> Irrelevant
    | QConstant (QProp | QType) -> Relevant
    | QConstant QGhost -> CIrrelevant
    | QVar qv' ->
      if qv' == qv then r else RelevanceVar qv'

let relevance_of_sort = function
  | SProp -> Irrelevant
  | Ghost _ -> CIrrelevant
  | Prop | Set | Type _ -> Relevant
  | QSort (q, _) -> RelevanceVar q

let debug_print = function
  | SProp -> Pp.(str "SProp")
  | Prop -> Pp.(str "Prop")
  | Set -> Pp.(str "Set")
  | Type u -> Pp.(str "Type(" ++ Univ.Universe.raw_pr u ++ str ")")
  | QSort (q, u) -> Pp.(str "QSort(" ++ QVar.raw_pr q ++ str ","
                        ++ spc() ++ Univ.Universe.raw_pr u ++ str ")")
  | Ghost u -> Pp.(str "Ghost@{" ++ Univ.Universe.raw_pr u ++ str "}")

let pr prv pru = function
  | SProp -> Pp.(str "SProp")
  | Prop -> Pp.(str "Prop")
  | Set -> Pp.(str "Set")
  | Type u -> Pp.(str "Type@{" ++ pru u ++ str "}")
  | QSort (q, u) -> Pp.(str "Type@{" ++ prv q ++ str "|"
                       ++ spc() ++ pru u ++ str "}")
  | Ghost u -> Pp.(str "Ghost@{" ++ pru u ++ str "}")

let raw_pr = pr QVar.raw_pr Univ.Universe.raw_pr

type pattern =
  | PSProp | PSSProp | PSSet | PSType of int option | PSQSort of int option * int option
  | PSGhost of int option

let extract_level u =
  match Universe.level u with
  | Some l -> l
  | None -> CErrors.anomaly Pp.(str "Tried to extract level of an algebraic universe")

let extract_sort_level = function
  | Type u | Ghost u
  | QSort (_, u) -> extract_level u
  | Prop | SProp | Set -> Univ.Level.set

let pattern_match ps s qusubst =
  match ps, s with
  | PSProp, Prop -> Some qusubst
  | PSSProp, SProp -> Some qusubst
  | PSSet, Set -> Some qusubst
  | PSType uio, Set -> Some (Partial_subst.maybe_add_univ uio Univ.Level.set qusubst)
  | PSType uio, Type u -> Some (Partial_subst.maybe_add_univ uio (extract_level u) qusubst)
  | PSGhost uio, Ghost u -> Some (Partial_subst.maybe_add_univ uio (extract_level u) qusubst)
  | PSQSort (qio, uio), s -> Some (qusubst |> Partial_subst.maybe_add_quality qio (quality s) |> Partial_subst.maybe_add_univ uio (extract_sort_level s))
  | (PSProp | PSSProp | PSSet | PSType _ | PSGhost _), _ -> None

let enforce_eq_quality a b csts =
  if Quality.equal a b then csts
  else ElimConstraints.add (a,ElimConstraint.Equal,b) csts

let enforce_elim_to_quality a b csts =
  if Quality.equal a b then csts
  else match a, b with
    | Quality.(QConstant QProp), Quality.(QConstant QType) -> csts
    | _ -> ElimConstraints.add (a,ElimConstraint.ElimTo,b) csts
