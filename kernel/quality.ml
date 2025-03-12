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

module QVar =
struct
  type repr =
    | Var of int
    | Unif of string * int

  type t = repr

  let make_var n = Var n

  let make_unif s n = Unif (s,n)

  let var_index = function
    | Var q -> Some q
    | Unif _ -> None

  let hash = function
    | Var q -> Hashset.Combine.combinesmall 1 q
    | Unif (s,q) -> Hashset.Combine.(combinesmall 2 (combine (CString.hash s) q))

  module Hstruct = struct
    type nonrec t = t
    type u = unit

    let hashcons () = function
      | Var _ as q -> q
      | Unif (s,i) as q ->
        let s' = CString.hcons s in
        if s == s' then q else Unif (s',i)

    let eq a b =
      match a, b with
      | Var a, Var b -> Int.equal a b
      | Unif (sa, ia), Unif (sb, ib) -> sa == sb && Int.equal ia ib
      | (Var _ | Unif _), _ -> false

    let hash = hash
  end

  module Hasher = Hashcons.Make(Hstruct)

  let hcons = Hashcons.simple_hcons Hasher.generate Hasher.hcons ()

  let compare a b = match a, b with
    | Var a, Var b -> Int.compare a b
    | Unif (s1,i1), Unif (s2,i2) ->
      let c = Int.compare i1 i2 in
      if c <> 0 then c
      else CString.compare s1 s2
    | Var _, Unif _ -> -1
    | Unif _, Var _ -> 1

  let equal a b = match a, b with
    | Var a, Var b ->  Int.equal a b
    | Unif (s1,i1), Unif (s2,i2) ->
      Int.equal i1 i2 && CString.equal s1 s2
    | Var _, Unif _ | Unif _, Var _ -> false

  let to_string = function
    | Var q -> Printf.sprintf "β%d" q
    | Unif (s,q) ->
      let s = if CString.is_empty s then "" else s^"." in
      Printf.sprintf "%sα%d" s q

  let raw_pr q = str (to_string q)

  let repr x = x
  let of_repr x = x

  module Self = struct type nonrec t = t let compare = compare end
  module Set = CSet.Make(Self)
  module Map = CMap.Make(Self)
end

type constant = QProp | QSProp | QType
type t = QVar of QVar.t | QConstant of constant
type quality = t

let var i = QVar (QVar.make_var i)

let var_index = function
  | QVar q -> QVar.var_index q
  | QConstant _ -> None

module Constants = struct
  let equal a b = match a, b with
    | QProp, QProp | QSProp, QSProp | QType, QType -> true
    | (QProp | QSProp | QType), _ -> false

  let compare a b = match a, b with
    | QSProp, QSProp -> 0
    | QSProp, _ -> -1
    | _, QSProp -> 1
    | QProp, QProp -> 0
    | QProp, _ -> -1
    | _, QProp -> 1
    | QType, QType -> 0

  let eliminates_to a b = match a, b with
    | _, QSProp -> true
    | (QProp | QType), QProp -> true
    | QType, _ -> true
    | _, _ -> false

  let to_string = function
    | QProp -> "Prop"
    | QSProp -> "SProp"
    | QType -> "Type"

  let pr q = str (to_string q)

  let hash = function
    | QSProp -> 0
    | QProp -> 1
    | QType -> 2

end

let equal a b = match a, b with
  | QVar a, QVar b -> QVar.equal a b
  | QConstant a, QConstant b -> Constants.equal a b
  | (QVar _ | QConstant _), _ -> false

let is_qsprop s = equal s (QConstant QSProp)
let is_qprop s = equal s (QConstant QProp)
let is_qtype s = equal s (QConstant QType)
let is_qvar s = match s with QVar _ -> true | _ -> false

let compare a b = match a, b with
  | QVar a, QVar b -> QVar.compare a b
  | QVar _, _ -> -1
  | _, QVar _ -> 1
  | QConstant a, QConstant b -> Constants.compare a b

let eliminates_to a b = match a, b with
  | QConstant QType, _ -> true
  | QVar q, QVar q' -> QVar.equal q q' (* FIXME *)
  | QConstant a, QConstant b -> Constants.eliminates_to a b
  | _, (QVar _ | QConstant _) -> false

let all_constants = [QConstant QSProp; QConstant QProp; QConstant QType]
let all = var 0 :: all_constants

let pr prv = function
  | QVar v -> prv v
  | QConstant q -> Constants.pr q

let raw_pr q = pr QVar.raw_pr q

let hash = let open Hashset.Combine in function
				    | QConstant q -> Constants.hash q
				    | QVar q -> combinesmall 3 (QVar.hash q)

let subst f = function
  | QConstant _ as q -> q
  | QVar qv as q ->
     match f qv with
     | QConstant _ as q -> q
     | QVar qv' as q' ->
        if qv == qv' then q else q'

let subst_fn m v =
  match QVar.Map.find_opt v m with
  | Some v -> v
  | None -> QVar v

module Hstruct = struct
  type nonrec t = t
  type u = QVar.t -> QVar.t

  let hashcons hv = function
    | QConstant _ as q -> q
    | QVar qv as q ->
       let qv' = hv qv in
       if qv == qv' then q else QVar qv'

  let eq a b =
    match a, b with
    | QVar a, QVar b -> a == b
    | QVar _, _ -> false
    | (QConstant _), _ -> equal a b

  let hash = hash
end

module Hasher = Hashcons.Make(Hstruct)

let hcons = Hashcons.simple_hcons Hasher.generate Hasher.hcons QVar.hcons

let qsprop = hcons (QConstant QSProp)
let qprop = hcons (QConstant QProp)
let qtype = hcons (QConstant QType)

module Self = struct type nonrec t = t let compare = compare end
module Set = CSet.Make(Self)
module Map = CMap.Make(Self)

type pattern =
  | PQVar of int option | PQConstant of constant

let pattern_match ps s qusubst =
  match ps, s with
  | PQConstant qc, QConstant qc' -> if Constants.equal qc qc' then Some qusubst else None
  | PQVar qio, q -> Some (Partial_subst.maybe_add_quality qio q qusubst)
  | PQConstant _, QVar _ -> None

let to_string = function
  | QConstant q -> Constants.to_string q
  | QVar q -> QVar.to_string q

module ElimConstraint = struct
  type kind = Eq | ElimTo | SElimTo

  let eq_kind : kind -> kind -> bool = (=)
  let compare_kind : kind -> kind -> int = Stdlib.compare

  let pr_kind = function
    | Eq -> str "="
    | ElimTo -> str "=>"
    | SElimTo -> str "->"

  type nonrec t = t * kind * t

  let trivial ((a,q,b) : t) =
    match q with
    | Eq | ElimTo -> equal a b
    | SElimTo -> false

  let equal (a,k,b) (a',k',b') =
    eq_kind k k' && equal a a' && equal b b'

  let compare (a,k,b) (a',k',b') =
    let c = compare_kind k k' in
    if c <> 0 then c
    else
      let c = compare a a' in
      if c <> 0 then c
      else compare b b'

  let pr prq (a,k,b) =
    hov 1 (pr prq a ++ spc() ++ pr_kind k ++ spc() ++ pr prq b)

  let raw_pr x = pr QVar.raw_pr x
end

module ElimConstraints =
struct
  module S = Stdlib.Set.Make(ElimConstraint)
  include S

  let trivial = for_all ElimConstraint.trivial

  let pr prv c =
    v 0 (prlist_with_sep spc (fun (u1,op,u2) ->
      hov 0 (pr prv u1 ++ ElimConstraint.pr_kind op ++ pr prv u2))
       (elements c))
end

module HElimConstraint =
  Hashcons.Make(
    struct
      type t = ElimConstraint.t
      type u = quality -> quality
      let hashcons hul (l1,k,l2) = (hul l1, k, hul l2)
      let eq (l1,k,l2) (l1',k',l2') =
        l1 == l1' && k == k' && l2 == l2'
      let hash = Hashtbl.hash
    end)

module HElimConstraints =
  Hashcons.Make(
    struct
      type t = ElimConstraints.t
      type u = ElimConstraint.t -> ElimConstraint.t
      let hashcons huc s =
        ElimConstraints.fold (fun x -> ElimConstraints.add (huc x)) s ElimConstraints.empty
      let eq s s' =
        List.for_all2eq (==)
          (ElimConstraints.elements s)
          (ElimConstraints.elements s')
      let hash = Hashtbl.hash
    end)

let hcons_elim_constraint =
  Hashcons.simple_hcons HElimConstraint.generate HElimConstraint.hcons hcons
let hcons_elim_constraints =
  Hashcons.simple_hcons HElimConstraints.generate HElimConstraints.hcons hcons_elim_constraint
