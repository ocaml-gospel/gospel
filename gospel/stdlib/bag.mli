type 'a bag

(*@ function nb_occ (x: 'a) (b: 'a bag): integer *)

(*@ axiom occ_non_negative: forall b: 'a bag, x: 'a.
      nb_occ x b >= 0 *)

(*@ predicate mem (x: 'a) (b: 'a bag) =
      nb_occ x b > 0 *)

(*@ predicate eq_bag (a b: 'a bag) =
      forall x:'a. nb_occ x a = nb_occ x b *)

(*@ axiom bag_extensionality: forall a b: 'a bag.
      eq_bag a b -> a = b *)

(*@ function empty_bag: 'a bag *)

(*@ axiom occ_empty: forall x: 'a. nb_occ x empty_bag = 0 *)

(*@ function singleton (x: 'a) : 'a bag *)

(*@ axiom occ_singleton: forall x y: 'a.
      nb_occ y (singleton x) = if x = y then 1 else 0 *)

(*@ function union (x:'a bag) (y:'a bag) : 'a bag *)

(* axiom occ_union: forall x: 'a, a b: 'a bag.
    nb_occ x (union a b) = nb_occ x a + nb_occ x b *)

  (** add operation *)

(*@ function add (x: 'a) (b: 'a bag) : 'a bag =
      union (singleton x) b *)

(** cardinality of bags *)

(*@ function card (x:'a bag): integer *)

(*@ axiom card_nonneg: forall x: 'a bag.
      card x >= 0 *)

(*@ axiom card_empty: card (empty_bag: 'a bag) = 0 *)

(*@ axiom card_zero_empty: forall x: 'a bag.
      card x = 0 -> x = empty_bag *)

(*@ axiom card_singleton: forall x:'a.
      card (singleton x) = 1 *)

(*@ axiom card_union: forall x y: 'a bag.
      card (union x y) = card x + card y *)

(** bag difference *)

(*@ function diff (x: 'a bag) (y: 'a bag) : 'a bag *)

(*@ axiom diff_occ: forall b1 b2: 'a bag, x:'a.
    nb_occ x (diff b1 b2) = max 0 (nb_occ x b1 - nb_occ x b2) *)

(** arbitrary element *)

(*@ function choose (b: 'a bag) : 'a *)

(*@ axiom choose_mem: forall b: 'a bag.
      empty_bag <> b -> mem (choose b) b *)
