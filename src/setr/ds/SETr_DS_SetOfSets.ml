let debug = false

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  (** [t] is a set<r> data structure *)
  type t

  (** [elt] is an element the sets *)
  type elt

  module ESet : Set.S with type elt = elt
  module EMap : Map.S with type key = elt

  (** [empty] returns the empty set of sets *)
  val empty : t

  (** [is_empty] returns true if this is an empty mapping (either empty or only
      singleton sets *)
  val is_empty : t -> bool

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  (* [remove e t] removes an element from the sets.  It returns an updated sets
     and a value that determines what happened to the representative:
     [NoRepresentative]    - There is no new representative
     [SameRepresentative]  - The representative did not change

     [NewRepresentative r] - The element removed was a representative and the
                             new representative for this set is [r] *)
  val remove : elt -> t -> t * rem

  (** [mem e t] returns true if [e] is in one of the sets explicitly described
      by [t] *)
  val mem : elt -> t -> bool

  (** [rep e t] returns the representative for the element [e] *)
  val rep : elt -> t -> elt

  (** [reps t] returns the set of all representatives *)
  val reps : t -> elt list

  (** [elements e t] returns the set of elements in the set with [e] within [t]
  *)
  val elements : elt -> t -> ESet.t

  (** [union e1 e2 t] unions the two sets [e1] and [e2] within [t], adding [e1] and [e2] if necessary *)
  val union : elt -> elt -> t -> t

  (** [merge t1 t2] produces [(t,r1,r2)].  [t] is the "meet" of [t1] and [t2].
      Precisely, the following is true of [t]:
        Forall s1, s2.  (rep s1 t1) = (rep s2 t1) => (rep s1 t) = (rep s2 t)
                   and  (rep s1 t2) = (rep s2 t2) => (rep s1 t) = (rep s2 t)

      [r1] and [r2] are (ideally minimal) tranformation mappings that can be
      applied to [t1] and [t2] respectively to make them represent the same
      set of sets as [t].  Precisely, the following is true:
        Exist bijection map1.  Forall s.  ((rep s t1), (rep s t)) in map1.
        and Exist bijection map2.  Forall s.  ((rep s t2), (rep s t)) in map2.
  *)
  val merge : t -> t -> t

  (** [split t1 t2] produces [(t,r1,r2)]. [t] is the "join" of [t1] and [t2].
      Precisely, the following is true of [t]:
        Forall s1, s2.  (rep s1 t1) = (rep s2 t1) => (rep s1 t) = (rep s2 t)
                    or  (rep s1 t2) = (rep s2 t2) => (rep s1 t) = (rep s2 t)

      [r1] and [r2] are (ideally minimal) tranformation mappings that can be
      applied to [t] to make it represent the same set of sets as [t1] and [t2]
      respectively.  Precisely, the following is true:
        Exist bijection map1.  Forall s.  ((rep 
  *)
  val split : t -> t -> t

  (** [diff t1 t2] takes two union-find structures [t1] and [t2], where [t1] is
      a refinement of [t2] (two elements MAY be in the same set in [t1] if they
      are in the same set in [t2].  It returns a list of pairs of elements that
      must be merged to transform [t1] into [t2]. *)
  val diff : t -> t -> (elt * elt) list

  (** [le t1 t2] queries if [t2] is a refinement of [t1].  That is if two
      elements are in the same set in [t2], they must be in the same set in
      [t1] *) val le : t -> t -> bool
  val fold : (elt -> elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** [rename map t] renames elements in [t] according to the mapping [map]. *)
  val rename : elt SETr_Rename.t -> t -> t

  (** [pairs t ] produces a list of all pairs needed to construct the
      union-find structure *)
  val pairs : t -> (elt * elt) list
end

module Make(C: Comparable) : S with type elt = C.t = struct
  module EMap = Map.Make(C)
  module ESet = Set.Make(C)

  type elt = C.t

  type t = {
    e2r: elt EMap.t;
    r2e: ESet.t EMap.t;
  }

  let empty = {
    e2r = EMap.empty;
    r2e = EMap.empty;
  }

  let is_empty t =
    EMap.is_empty t.e2r ||
    EMap.for_all (fun e r -> C.compare e r = 0) t.e2r

  let mem e t =
    EMap.mem e t.e2r

  type rem =
    | NoRepresentative
    | SameRepresentative
    | NewRepresentative of elt

  let remove e t =
    try
      let r = EMap.find e t.e2r in
      if C.compare e r = 0 then
        (* removing representative *)
        let es = EMap.find r t.r2e in
        let es = ESet.remove r es in
        if ESet.is_empty es then
          (* removed sole element *)
          ({ r2e = EMap.remove r t.r2e; e2r = EMap.remove e t.e2r}, NoRepresentative)
        else
          let r2e = EMap.remove r t.r2e in
          (* select new representative *)
          let r = ESet.min_elt es in
          let r2e = EMap.add r es r2e in
          (* update set to point to new representative *)
          let e2r = EMap.remove e t.e2r in
          let e2r = ESet.fold (fun e e2r -> EMap.add e r e2r) es e2r in
          ({ r2e; e2r }, NewRepresentative r)
      else
        (* removing non-representative *)
        let es = EMap.find r t.r2e in
        let es = ESet.remove e es in
        let r2e = EMap.add r es t.r2e in
        let e2r = EMap.remove e t.e2r in
        ({ r2e; e2r }, SameRepresentative)
    with Not_found ->
      (t, NoRepresentative)

  let rep e t =
    try EMap.find e t.e2r with Not_found -> e

  let reps t =
    EMap.bindings t.r2e |>
    List.map fst

  let element_set e t =
    try
      EMap.find (rep e t) t.r2e
    with Not_found ->
      ESet.singleton e

  let elements e t =
    element_set e t

  let union e1 e2 t =
    let r1 = rep e1 t in
    let r2 = rep e2 t in
    let (r1,r2) = if C.compare r1 r2 > 0 then (r2,r1) else (r1,r2) in
    (* r1 becomes new representative *)
    let r1s = element_set r1 t in
    let r2s = element_set r2 t in
    let s = ESet.union r1s r2s in
    let r2e = EMap.remove r2 t.r2e in
    let r2e = EMap.add r1 s r2e in
    let e2r = ESet.fold (fun e e2r -> EMap.add e r1 e2r) s t.e2r in
    { e2r; r2e }

  module CEMap = Map.Make(struct
      type t = C.t * C.t
      let compare (a1,a2) (b1,b2) =
        let res = C.compare a1 b1 in
        if res <> 0 then res
        else C.compare a2 b2
    end)

  let compare t1 t2 =
    let res = EMap.compare C.compare t1.e2r t2.e2r in
    if res <> 0 then res
    else EMap.compare ESet.compare t1.r2e t2.r2e

  let equal t1 t2 =
    compare t1 t2 = 0

  let fold f t r =
    EMap.fold f t.e2r r

  let merge t1 t2 =
    let res = t1 in
    let res = EMap.fold (fun e r res ->
        res |>
        union e r
      ) t2.e2r res in
    res

  let split t1 t2 =
    let h = Hashtbl.create 8191 in
    let res = ref empty in
    ignore(EMap.merge (fun e r1 r2 ->
        let r1 = match r1 with Some r1 -> r1 | None -> e in
        let r2 = match r2 with Some r2 -> r2 | None -> e in
        begin try
            let r = Hashtbl.find h (r1,r2) in
            res := union e r !res
          with Not_found ->
            Hashtbl.replace h (r1,r2) e;
            res := union e e !res
        end;
        None
      ) t1.e2r t2.e2r);
    !res

  let diff t1 t2 =
    snd (fold (fun e r (t1,tm) ->
        let r' = rep e t1 in
        if C.compare r e <> 0 && C.compare r' e = 0 then
          (union e r t1, (e,r)::tm)
        else
          (t1,tm)
      ) t2 (t1,[]))

  let rename rename t =
    fold (fun e r t ->
        let e = SETr_Rename.get rename e in
        let r = SETr_Rename.get rename r in
        union e r t
      ) t empty

  let pairs t =
    EMap.bindings t.e2r


  (* This is true if there exists a function f that maps each representative in
     t2 to each representative in t1 such that
       forall e.  f(rep(e,t2)) = rep(e,t1)

     For example (representative in parens):
       a = (b), (c) = d   <=   (a) = b, (c), (d)

     The function f can be defined as such
     f(a) = b
     f(c) = c
     f(d) = c

     rep(a,t2) = a
     |> f      = b
     rep(a,t1) = b

     rep(b,t2) = a
     |> f      = b
     rep(b,t1) = b

     rep(c,t2) = c
     |> f      = c
     rep(c,t1) = c

     rep(d,t2) = d
     |> f      = c
     rep(d,t1) = c
  *)
  let le t1 t2 =
    let h = Hashtbl.create (3*(EMap.cardinal t2.r2e)) in
    EMap.for_all (fun e r2 ->
        let r1 = rep e t1 in
        try
          let r1' = Hashtbl.find h r2 in
          r1 == r1'
        with Not_found ->
          Hashtbl.replace h r2 r1;
          true
      ) t2.e2r

end
