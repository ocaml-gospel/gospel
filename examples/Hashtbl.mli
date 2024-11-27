module type H = sig
  (*@ open Sequence *)
  (*@ open Map *)

  type ('a, 'b) t
  (*@ ephemeral
      model : 'a -> 'b sequence *)

  val create : int -> ('a, 'b) t
  (*@ tbl = create n
        produces tbl @ ('a, 'b) t
        ensures tbl = fun _ -> empty *)

  val clear : ('a, 'b) t -> ('a, 'b) t
  (*@ clear tbl
        modifies tbl @ ('a, 'b) t
        ensures tbl = fun _ -> empty *)

  val copy : ('a, 'b) t -> ('a, 'b) t
  (*@ c = copy tbl
        preserves tbl @ ('a, 'b) t
        ensures tbl = c *)

  val add : ('a, 'b) t -> 'a -> 'b -> unit
  (*@ add tbl k v
        modifies tbl @ ('a, 'b) t
        ensures tbl = old (tbl[k -> cons v (tbl k)]) *)

  val find_opt : ('a, 'b) t -> 'a -> 'b option
  (*@ r = find_opt tbl k
        preserves tbl @ ('a, 'b) t
        ensures match r with
        |None -> tbl k = empty
        |Some x -> hd (tbl k) = x *)

  val find_all : ('a, 'b) t -> 'a -> 'b list
  (*@ l = find_all tbl k
        preserves tbl @ ('a, 'b) t
        ensures l = tbl k *)

  val mem : ('a, 'b) t -> 'a -> bool
  (*@ b = mem tbl k
        preserves tbl @ ('a, 'b) t
        ensures b <-> tbl k <> empty *)

  val remove : ('a, 'b) t -> 'a -> unit
  (*@ remove tbl k
        modifies tbl @ ('a, 'b) t
        ensures old (tbl k) = empty -> tbl = old tbl
        ensures tbl k <> empty -> tbl = old (tbl[k -> tl (tbl k)]) *)

  val replace : ('a, 'b) t -> 'a -> 'b -> unit
  (*@ replace tbl k v
        modifies tbl @ ('a, 'b) t
        ensures tbl k = empty -> tbl = tbl[k -> singleton v]
        ensures tbl k <> empty ->
        let tail = old (tl (tbl k)) in
        tbl = old (tbl[k -> cons v tail]) *)

  (*@ function domain (default : 'b) (m : 'a -> 'b) : 'a Set.t *)
  (*@ axiom domain_mem :
        forall x m default. m x <> default -> Set.mem x (domain default m) *)

  val length : ('a, 'b) t -> int
  (*@ n = length tbl
        preserves tbl @ ('a, 'b) t
        ensures n = Set.cardinal (domain empty tbl) *)
end
