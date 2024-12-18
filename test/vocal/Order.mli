(*@ predicate is_pre_order (cmp: 'a -> 'a -> int) =
      (forall x. cmp x x = 0) /\
      (forall x y. cmp x y <= 0 <-> cmp y x >= 0) /\
      (forall x y z.
         (cmp x y <= 0 -> cmp y z <= 0 -> cmp x z <= 0) /\
         (cmp x y <= 0 -> cmp y z <  0 -> cmp x z <  0) /\
         (cmp x y <  0 -> cmp y z <= 0 -> cmp x z <  0) /\
         (cmp x y <  0 -> cmp y z <  0 -> cmp x z <  0)) *)
