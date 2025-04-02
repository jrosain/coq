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
open UVars
open UnivConstraint

module G = AcyclicGraph.Make(struct
    type t = Level.t
    module Set = Level.Set
    module Map = Level.Map

    let equal = Level.equal
    let compare = Level.compare

    let raw_pr = Level.raw_pr

    let anomaly_label = "Univ.repr"
    let anomaly_err u = Pp.(str"Universe " ++ Level.raw_pr u ++ str" undefined.")
  end)
(* Do not include G to make it easier to control universe specific
   code (eg add_universe with a constraint vs G.add with no
   constraint) *)

type t = {
  graph: G.t;
  type_in_type : bool;
}

(* Universe inconsistency: error raised when trying to enforce a relation
   that would create a cycle in the graph of universes. *)

type path_explanation = G.explanation Lazy.t

type explanation =
  | Path of path_explanation
  | Other of Pp.t

type univ_variable_printers = (Quality.QVar.t -> Pp.t) * (Level.t -> Pp.t)
type univ_inconsistency = univ_variable_printers option * (UnivConstraint.kind * Sorts.t * Sorts.t * explanation option)

exception UniverseInconsistency of univ_inconsistency

type 'a check_function = t -> 'a -> 'a -> bool

let set_type_in_type b g = {g with type_in_type=b}

let type_in_type g = g.type_in_type

let check_smaller_expr g (u,n) (v,m) =
  let diff = n - m in
    match diff with
    | 0 -> G.check_leq g.graph u v
    | 1 -> G.check_lt g.graph u v
    | x when x < 0 -> G.check_leq g.graph u v
    | _ -> false

let exists_bigger g ul l =
  Universe.exists (fun ul' ->
    check_smaller_expr g ul ul') l

let real_check_leq g u v =
  Universe.for_all (fun ul -> exists_bigger g ul v) u

let check_leq g u v =
  type_in_type g || Universe.equal u v || (real_check_leq g u v)

let check_eq g u v =
  type_in_type g || Universe.equal u v ||
    (real_check_leq g u v && real_check_leq g v u)

let check_eq_level g u v =
  u == v || type_in_type g || G.check_eq g.graph u v

let empty_universes = {
  graph=G.empty;
  type_in_type=false;
}

let initial_universes =
  let big_rank = 1000000 in
  let g = G.empty in
  let g = G.add ~rank:big_rank Level.set g in
  {empty_universes with graph=g}

let initial_universes_with g = {g with graph=initial_universes.graph}

let enforce_constraint (u,d,v) g =
  match d with
  | Le -> G.enforce_leq u v g
  | Lt -> G.enforce_lt u v g
  | Eq -> G.enforce_eq u v g

let enforce_constraint0 cst g = match enforce_constraint cst g.graph with
| None -> None
| Some g' ->
  if g' == g.graph then Some g
  else Some { g with graph = g' }

let enforce_constraint cst g = match enforce_constraint0 cst g with
| None ->
  if not (type_in_type g) then
    let (u, c, v) = cst in
    let e = lazy (G.get_explanation cst g.graph) in
    let mk u = Sorts.sort_of_univ @@ Universe.make u in
    raise (UniverseInconsistency (None, (c, mk u, mk v, Some (Path e))))
  else g
| Some g -> g

let merge_constraints csts g = UnivConstraints.fold enforce_constraint csts g

let check_constraint { graph = g; type_in_type } (u,d,v) =
  type_in_type
  || match d with
  | Le -> G.check_leq g u v
  | Lt -> G.check_lt g u v
  | Eq -> G.check_eq g u v

let check_constraints csts g = UnivConstraints.for_all (check_constraint g) csts

let check_eq_sort quals univs s1 s2 =
  (* if type_in_type, we only care about relevance *)
  if type_in_type univs then
    let r1 = Sorts.relevance_of_sort s1 in
    let r2 = Sorts.relevance_of_sort s2 in
    Sorts.relevance_equal r1 r2
  else
    let u1 = Sorts.univ_of_sort s1 in
    let u2 = Sorts.univ_of_sort s2 in
    QGraph.check_eq_sort quals s1 s2 &&
      check_eq univs u1 u2

let check_leq_sort quals univs s1 s2 =
  (* FIXME: how do we compare relevances in this case? *)
  if type_in_type univs then true
  else
    let u1 = Sorts.univ_of_sort s1 in
    let u2 = Sorts.univ_of_sort s2 in
    let r1 = Sorts.relevance_of_sort s1 in
    let r2 = Sorts.relevance_of_sort s2 in
    let open Sorts in
    let elim_prop q = QGraph.eliminates_to_prop quals (Quality.QVar q) in
    (Sorts.relevance_equal r1 r2 && check_leq univs u1 u2) ||
      match s1, s2 with
      | Prop, QSort (q, _) | QSort (q, _), Set -> elim_prop q
      | QSort (q, _), Type _ -> elim_prop q && check_leq univs u1 u2
      | _ -> false

let leq_expr (u,m) (v,n) =
  let d = match m - n with
    | 1 -> Lt
    | diff -> assert (diff <= 0); Le
  in
  (u,d,v)

let enforce_leq_alg u v g =
  let open Util in
  let enforce_one (u,v) = function
    | Inr _ as orig -> orig
    | Inl (cstrs,g) as orig ->
      if check_smaller_expr g u v then orig
      else
        (let c = leq_expr u v in
         match enforce_constraint0 c g with
         | Some g -> Inl (UnivConstraints.add c cstrs,g)
         | None -> Inr (c, g))
  in
  (* max(us) <= max(vs) <-> forall u in us, exists v in vs, u <= v *)
  let c = List.map (fun u -> List.map (fun v -> (u,v)) (Universe.repr v)) (Universe.repr u) in
  let c = List.cartesians enforce_one (Inl (UnivConstraints.empty,g)) c in
  (* We pick a best constraint: smallest number of constraints, not an error if possible. *)
  let order x y = match x, y with
    | Inr _, Inr _ -> 0
    | Inl _, Inr _ -> -1
    | Inr _, Inl _ -> 1
    | Inl (c,_), Inl (c',_) ->
      Int.compare (UnivConstraints.cardinal c) (UnivConstraints.cardinal c')
  in
  match List.min order c with
  | Inl x -> x
  | Inr ((u, c, v), g) ->
    let e = lazy (G.get_explanation (u, c, v) g.graph) in
    let mk u = Sorts.sort_of_univ @@ Universe.make u in
    let e = UniverseInconsistency (None, (c, mk u, mk v, Some (Path e))) in
    raise e

exception AlreadyDeclared = G.AlreadyDeclared
let add_universe u ~strict g =
  let graph = G.add u g.graph in
  let d = if strict then Lt else Le in
  enforce_constraint (Level.set, d, u) { g with graph }

let check_declared_universes g l =
  G.check_declared g.graph l

let constraints_of_universes g =
  let add cst accu = UnivConstraints.add cst accu in
  G.constraints_of g.graph add UnivConstraints.empty
let constraints_for ~kept g =
  let add cst accu = UnivConstraints.add cst accu in
  G.constraints_for ~kept g.graph add UnivConstraints.empty

(** Subtyping of polymorphic contexts *)

let check_subtype univs ctxT ctx =
  (* NB: size check is the only constraint on qualities *)
  if eq_sizes (AbstractContext.size ctxT) (AbstractContext.size ctx) then
    let uctx = AbstractContext.repr ctx in
    let inst = UContext.instance uctx in
    let cst = UContext.constraints uctx in
    let cstT = UContext.constraints (AbstractContext.repr ctxT) in
    let push accu v = add_universe v ~strict:false accu in
    let univs = Array.fold_left push univs (snd (Instance.to_array inst)) in
    let univs = merge_constraints cstT univs in
    check_constraints cst univs
  else false

(** Instances *)

let check_eq_instances g t1 t2 =
  let qt1, ut1 = Instance.to_array t1 in
  let qt2, ut2 = Instance.to_array t2 in
  CArray.equal Quality.equal qt1 qt2
  && CArray.equal (check_eq_level g) ut1 ut2

let domain g = G.domain g.graph
let choose p g u = G.choose p g.graph u

let check_universes_invariants g = G.check_invariants ~required_canonical:Level.is_set g.graph

(** Pretty-printing *)

let pr_pmap sep pr map =
  let cmp (u,_) (v,_) = Level.compare u v in
  Pp.prlist_with_sep sep pr (List.sort cmp (Level.Map.bindings map))

let pr_arc prl = let open Pp in
  function
  | u, G.Node ltle ->
    if Level.Map.is_empty ltle then mt ()
    else
      prl u ++ str " " ++
      v 0
        (pr_pmap spc (fun (v, strict) ->
              (if strict then str "< " else str "<= ") ++ prl v)
            ltle) ++
      fnl ()
  | u, G.Alias v ->
    prl u  ++ str " = " ++ prl v ++ fnl ()

type node = G.node =
| Alias of Level.t
| Node of bool Level.Map.t

let repr g = G.repr g.graph

let pr_universes prl g = pr_pmap Pp.mt (pr_arc prl) g

open Pp

let explain_universe_inconsistency default_prq default_prl (printers, (o,u,v,p) : univ_inconsistency) =
  let prq, prl = match printers with
    | Some (prq, prl) -> prq, prl
    | None -> default_prq, default_prl
  in
  let pr_uni u = match u with
  | Sorts.Set -> str "Set"
  | Sorts.Prop -> str "Prop"
  | Sorts.SProp -> str "SProp"
  | Sorts.Type u -> Universe.pr prl u
  | Sorts.QSort (q, u) -> str "Type@{" ++ prq q ++ str " | " ++ Universe.pr prl u ++ str"}"
  in
  let pr_rel = function
    | Eq -> str"=" | Lt -> str"<" | Le -> str"<="
  in
  let reason = match p with
    | None -> mt()
    | Some (Other p) -> spc() ++ p
    | Some (Path p) ->
      let pstart, p = Lazy.force p in
      if p = [] then mt ()
      else
        str " because" ++ spc() ++ prl pstart ++
        prlist (fun (r,v) -> spc() ++ pr_rel r ++ str" " ++ prl v) p
  in
    str "Cannot enforce" ++ spc() ++ pr_uni u ++ spc() ++
      pr_rel o ++ spc() ++ pr_uni v ++ reason
