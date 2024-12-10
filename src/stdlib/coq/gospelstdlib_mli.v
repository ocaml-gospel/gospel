(* Automatically generated file *)

Set Implicit Arguments.

From TLC Require Import LibString LibList LibCore.

Require Import Coq.ZArith.BinInt TLC.LibLogic TLC.LibRelation TLC.LibInt.

Require Import Coq.ZArith.BinIntDef.

Delimit Scope Z_scope with Z.

Module Type Stdlib.

Parameter sequence : Type -> Type.

Parameter bag : Type -> Type.

Parameter set : Type -> Type.

Parameter map : Type -> Type -> Type.

Parameter succ : Z -> Z.

Parameter pred : Z -> Z.

Parameter neg : Z -> Z.

Parameter plus : Z -> Z -> Z.

Parameter minus : Z -> Z -> Z.

Parameter mult : Z -> Z -> Z.

Parameter div : Z -> Z -> Z.

Parameter _mod : Z -> Z -> Z.

Parameter pow : Z -> Z -> Z.

Parameter abs : Z -> Z.

Parameter min : Z -> Z -> Z.

Parameter max : Z -> Z -> Z.

Parameter gt : Z -> Z -> Prop.

Parameter ge : Z -> Z -> Prop.

Parameter lt : Z -> Z -> Prop.

Parameter le : Z -> Z -> Prop.

Parameter max_int : Z.

Parameter min_int : Z.

Parameter app :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a -> sequence a.

Parameter seq_get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> a.

Parameter seq_sub :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> Z -> sequence a.

Parameter seq_sub_l :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> sequence a.

Parameter seq_sub_r :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> sequence a.

Module Sequence.

Parameter t : Type -> Type.

Parameter length :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z.

Parameter in_range :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> Prop.

Axiom in_range_def :
  forall {a4 : Type},
  forall {Ih_a4 : Inhab a4},
  forall s : sequence a4,
  forall i : Z,
  Coq.Init.Logic.iff (in_range s i) (in_range s i).

Axiom length_nonneg :
  forall {a6 : Type},
  forall {Ih_a6 : Inhab a6},
  forall s : sequence a6,
  le (0)%Z (length s).

Axiom subseq_l :
  forall {a12 : Type},
  forall {Ih_a12 : Inhab a12},
  forall s : sequence a12,
  forall i : Z,
  in_range s i -> eq (seq_sub_l s i) (seq_sub s i (length s)).

Axiom subseq_r :
  forall {a18 : Type},
  forall {Ih_a18 : Inhab a18},
  forall s : sequence a18,
  forall i : Z,
  in_range s i -> eq (seq_sub_r s i) (seq_sub s (0)%Z i).

Axiom subseq :
  forall {a28 : Type},
  forall {Ih_a28 : Inhab a28},
  forall s : sequence a28,
  forall i : Z,
  forall i1 : Z,
  forall i2 : Z,
  Coq.Init.Logic.and (le (0)%Z i1) (
    Coq.Init.Logic.and (le i1 i) (
      Coq.Init.Logic.and (lt i i2) (le i2 (length s))
    )
  ) ->
  eq (seq_get s i) (seq_get (seq_sub s i1 i2) (minus i i1)).

Axiom subseq_len :
  forall {a34 : Type},
  forall {Ih_a34 : Inhab a34},
  forall s : sequence a34,
  forall i1 : Z,
  forall i2 : Z,
  Coq.Init.Logic.and (le (0)%Z i1) (
    Coq.Init.Logic.and (le i1 i2) (lt i2 (length s))
  ) ->
  eq (length (seq_sub s i1 i2)) (minus i2 i1).

Parameter empty :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a.

Axiom empty_length :
  forall {a37 : Type},
  forall {Ih_a37 : Inhab a37},
  eq (length (@empty a37 Ih_a37)) (0)%Z.

Parameter init :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  Z -> (Z -> a) -> sequence a.

Axiom init_length :
  forall {a42 : Type},
  forall {Ih_a42 : Inhab a42},
  forall n : Z,
  forall f : Z -> a42,
  ge n (0)%Z -> eq (length (init n f)) n.

Axiom init_elems :
  forall {a50 : Type},
  forall {Ih_a50 : Inhab a50},
  forall n : Z,
  forall f : Z -> a50,
  forall i : Z,
  Coq.Init.Logic.and (le (0)%Z i) (lt i n) ->
  eq (seq_get (init n f) i) (f i).

Parameter singleton :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a.

Axiom singleton_def :
  forall {a56 : Type},
  forall {Ih_a56 : Inhab a56},
  forall x : a56,
  forall f : Z -> a56,
  eq (f (0)%Z) x -> eq (singleton x) (init (1)%Z f).

Parameter cons :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a -> sequence a.

Axiom cons_def :
  forall {a62 : Type},
  forall {Ih_a62 : Inhab a62},
  forall x : a62,
  forall s : sequence a62,
  eq (cons x s) (app (singleton x) s).

Parameter snoc :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> a -> sequence a.

Axiom snoc_def :
  forall {a68 : Type},
  forall {Ih_a68 : Inhab a68},
  forall s : sequence a68,
  forall x : a68,
  eq (snoc s x) (app s (singleton x)).

Parameter hd :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> a.

Axiom hd_def :
  forall {a73 : Type},
  forall {Ih_a73 : Inhab a73},
  forall s : sequence a73,
  eq (hd s) (seq_get s (0)%Z).

Parameter tl :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a.

Axiom tl_def :
  forall {a76 : Type},
  forall {Ih_a76 : Inhab a76},
  forall s : sequence a76,
  eq (tl s) (seq_sub_l s (1)%Z).

Parameter append :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a -> sequence a.

Axiom append_def :
  forall {a81 : Type},
  forall {Ih_a81 : Inhab a81},
  forall s1 : sequence a81,
  forall s2 : sequence a81,
  eq (append s1 s2) (app s1 s2).

Axiom append_length :
  forall {a88 : Type},
  forall {Ih_a88 : Inhab a88},
  forall s : sequence a88,
  forall s' : sequence a88,
  eq (length (app s s')) (plus (length s) (length s')).

Axiom append_elems_left :
  forall {a97 : Type},
  forall {Ih_a97 : Inhab a97},
  forall s : sequence a97,
  forall s' : sequence a97,
  forall i : Z,
  in_range s i -> eq (seq_get (app s s') i) (seq_get s i).

Axiom append_elems_right :
  forall {a108 : Type},
  forall {Ih_a108 : Inhab a108},
  forall s : sequence a108,
  forall s' : sequence a108,
  forall i : Z,
  Coq.Init.Logic.and (le (length s) i) (
    lt i (plus (length s) (length s'))
  ) ->
  eq (seq_get (app s s') i) (seq_get s' (minus i (length s))).

Parameter multiplicity :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a -> Z.

Axiom mult_empty :
  forall {a111 : Type},
  forall {Ih_a111 : Inhab a111},
  forall x : a111,
  eq (multiplicity x (@empty a111 Ih_a111)) (0)%Z.

Axiom mult_cons :
  forall {a117 : Type},
  forall {Ih_a117 : Inhab a117},
  forall s : sequence a117,
  forall x : a117,
  eq (plus (1)%Z (multiplicity x s)) (multiplicity x (cons x s)).

Axiom mult_cons_neutral :
  forall {a125 : Type},
  forall {Ih_a125 : Inhab a125},
  forall s : sequence a125,
  forall x1 : a125,
  forall x2 : a125,
  Coq.Init.Logic.not (eq x1 x2) ->
  eq (multiplicity x1 s) (multiplicity x1 (cons x2 s)).

Axiom mult_length :
  forall {a130 : Type},
  forall {Ih_a130 : Inhab a130},
  forall x : a130,
  forall s : sequence a130,
  Coq.Init.Logic.and (le (0)%Z (multiplicity x s)) (
    le (multiplicity x s) (length s)
  ).

Parameter mem :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a -> Prop.

Axiom mem_def :
  forall {a134 : Type},
  forall {Ih_a134 : Inhab a134},
  forall s : sequence a134,
  forall x : a134,
  Coq.Init.Logic.iff (mem x s) (gt (multiplicity x s) (0)%Z).

Parameter map :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b) -> sequence a -> sequence b.

Axiom map_elems :
  forall {a142 : Type},
  forall {a144 : Type},
  forall {Ih_a142 : Inhab a142},
  forall {Ih_a144 : Inhab a144},
  forall i : Z,
  forall f : a142 -> a144,
  forall s : sequence a142,
  in_range s i -> eq (seq_get (map f s) i) (f (seq_get s i)).

Parameter filter :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Prop) -> sequence a -> sequence a.

Axiom filter_elems :
  forall {a151 : Type},
  forall {Ih_a151 : Inhab a151},
  forall f : a151 -> bool,
  forall s : sequence a151,
  forall x : a151,
  mem x s -> f x -> mem x (filter f s).

Parameter filter_map :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> option b) -> sequence a -> sequence b.

Axiom filter_map_elems :
  forall {a707 : Type},
  forall {a708 : Type},
  forall {Ih_a707 : Inhab a707},
  forall {Ih_a708 : Inhab a708},
  forall f : a707 -> option a708,
  forall s : sequence a707,
  forall y : a708,
    (exists x, f x = Some y /\ mem x s) <->
      mem y (filter_map f s).

Parameter get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> a.

Axiom get_def :
  forall {a156 : Type},
  forall {Ih_a156 : Inhab a156},
  forall s : sequence a156,
  forall i : Z,
  eq (get s i) (seq_get s i).

Parameter set :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Z -> a -> sequence a.

Axiom set_elem :
  forall {a163 : Type},
  forall {Ih_a163 : Inhab a163},
  forall s : sequence a163,
  forall i : Z,
  forall x : a163,
  in_range s i -> eq (seq_get (set s i x) i) x.

Axiom set_elem_other :
  forall {a174 : Type},
  forall {Ih_a174 : Inhab a174},
  forall s : sequence a174,
  forall i1 : Z,
  forall i2 : Z,
  forall x : a174,
  Coq.Init.Logic.not (eq i1 i2) ->
  in_range s i1 ->
  in_range s i2 -> eq (seq_get (set s i1 x) i2) (seq_get s i2).

Parameter rev :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a.

Axiom rev_length :
  forall {a178 : Type},
  forall {Ih_a178 : Inhab a178},
  forall s : sequence a178,
  eq (length s) (length (rev s)).

Axiom rev_elems :
  forall {a187 : Type},
  forall {Ih_a187 : Inhab a187},
  forall i : Z,
  forall s : sequence a187,
  in_range s i ->
  eq (seq_get (rev s) i) (
    seq_get s (minus (minus (length s) (1)%Z) i)
  ).

Axiom extensionality :
  forall {a197 : Type},
  forall {Ih_a197 : Inhab a197},
  forall s1 : sequence a197,
  forall s2 : sequence a197,
  eq (length s1) (length s2) ->
  (
    forall i : Z,
    in_range s1 i -> eq (seq_get s1 i) (seq_get s2 i)
  ) ->
  eq s1 s2.

Parameter fold_left :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_a : Inhab b},
  (a -> b -> a) -> a -> sequence b -> a.

Axiom fold_left_empty :
  forall {a203 : Type},
  forall {a204 : Type},
  forall {Ih_a203 : Inhab a203},
  forall {Ih_a204 : Inhab a204},
  forall f : a204 -> a203 -> a204,
  forall acc : a204,
  eq (fold_left f acc (@empty a203 Ih_a203)) acc.

Axiom fold_left_cons :
  forall {a215 : Type},
  forall {a216 : Type},
  forall {Ih_a215 : Inhab a215},
  forall {Ih_a216 : Inhab a216},
  forall f : a216 -> a215 -> a216,
  forall acc : a216,
  forall x : a215,
  forall l : sequence a215,
  eq (fold_left f acc (cons x l)) (fold_left f (f acc x) l).

Parameter fold_right :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b -> b) -> sequence a -> b -> b.

Axiom fold_right_empty :
  forall {a221 : Type},
  forall {a222 : Type},
  forall {Ih_a221 : Inhab a221},
  forall {Ih_a222 : Inhab a222},
  forall acc : a222,
  forall f : a221 -> a222 -> a222,
  eq (fold_right f (@empty a221 Ih_a221) acc) acc.

Axiom fold_right_cons :
  forall {a232 : Type},
  forall {a234 : Type},
  forall {Ih_a232 : Inhab a232},
  forall {Ih_a234 : Inhab a234},
  forall acc : a234,
  forall f : a232 -> a234 -> a234,
  forall x : a232,
  forall l : sequence a232,
  eq (fold_right f (cons x l) acc) (f x (fold_right f l acc)).

Parameter of_list :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  list a -> sequence a.

End Sequence.

Module Bag.

Parameter t : Type -> Type.

Parameter multiplicity :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a -> Z.

Axiom well_formed :
  forall {a237 : Type},
  forall {Ih_a237 : Inhab a237},
  forall b : bag a237,
  forall x : a237,
  ge (multiplicity x b) (0)%Z.

Parameter empty : forall {a : Type}, forall {Ih_a : Inhab a}, bag a.

Axiom empty_mult :
  forall {a240 : Type},
  forall {Ih_a240 : Inhab a240},
  forall x : a240,
  eq (multiplicity x (@empty a240 Ih_a240)) (0)%Z.

Parameter init :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Z) -> bag a.

Axiom init_axiom :
  forall {a246 : Type},
  forall {Ih_a246 : Inhab a246},
  forall f : a246 -> Z,
  forall x : a246,
  eq (max (0)%Z (f x)) (multiplicity x (init f)).

Parameter mem :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a -> Prop.

Axiom mem_def :
  forall {a251 : Type},
  forall {Ih_a251 : Inhab a251},
  forall x : a251,
  forall b : bag a251,
  Coq.Init.Logic.iff (mem x b) (gt (multiplicity x b) (0)%Z).

Parameter add :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a -> bag a.

Axiom add_mult_x :
  forall {a256 : Type},
  forall {Ih_a256 : Inhab a256},
  forall b : bag a256,
  forall x : a256,
  eq (multiplicity x (add x b)) (plus (1)%Z (multiplicity x b)).

Axiom add_mult_neg_x :
  forall {a264 : Type},
  forall {Ih_a264 : Inhab a264},
  forall x : a264,
  forall y : a264,
  forall b : bag a264,
  Coq.Init.Logic.not (eq x y) ->
  eq (multiplicity y (add x b)) (multiplicity y b).

Parameter singleton :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a.

Axiom singleton_def :
  forall {a269 : Type},
  forall {Ih_a269 : Inhab a269},
  forall x : a269,
  eq (singleton x) (add x (@empty a269 Ih_a269)).

Parameter remove :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a -> bag a.

Axiom remove_mult_x :
  forall {a275 : Type},
  forall {Ih_a275 : Inhab a275},
  forall b : bag a275,
  forall x : a275,
  eq (multiplicity x (remove x b)) (
    max (0)%Z (minus (multiplicity x b) (1)%Z)
  ).

Axiom remove_mult_neg_x :
  forall {a283 : Type},
  forall {Ih_a283 : Inhab a283},
  forall x : a283,
  forall y : a283,
  forall b : bag a283,
  Coq.Init.Logic.not (eq x y) ->
  eq (multiplicity y (remove x b)) (multiplicity y b).

Parameter union :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> bag a.

Axiom union_all :
  forall {a291 : Type},
  forall {Ih_a291 : Inhab a291},
  forall b : bag a291,
  forall b' : bag a291,
  forall x : a291,
  eq (max (multiplicity x b) (multiplicity x b')) (
    multiplicity x (union b b')
  ).

Parameter sum :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> bag a.

Axiom sum_all :
  forall {a299 : Type},
  forall {Ih_a299 : Inhab a299},
  forall b : bag a299,
  forall b' : bag a299,
  forall x : a299,
  eq (plus (multiplicity x b) (multiplicity x b')) (
    multiplicity x (sum b b')
  ).

Parameter inter :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> bag a.

Axiom inter_all :
  forall {a307 : Type},
  forall {Ih_a307 : Inhab a307},
  forall b : bag a307,
  forall b' : bag a307,
  forall x : a307,
  eq (min (multiplicity x b) (multiplicity x b')) (
    multiplicity x (inter b b')
  ).

Parameter disjoint :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> Prop.

Axiom disjoint_def :
  forall {a314 : Type},
  forall {Ih_a314 : Inhab a314},
  forall b : bag a314,
  forall b' : bag a314,
  Coq.Init.Logic.iff (disjoint b b') (
    forall x : a314,
    mem x b -> Coq.Init.Logic.not (mem x b')
  ).

Parameter diff :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> bag a.

Axiom diff_all :
  forall {a321 : Type},
  forall {Ih_a321 : Inhab a321},
  forall b : bag a321,
  forall b' : bag a321,
  forall x : a321,
  eq (max (0)%Z (minus (multiplicity x b) (multiplicity x b'))) (
    multiplicity x (diff b b')
  ).

Parameter subset :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> bag a -> Prop.

Axiom subset_def :
  forall {a328 : Type},
  forall {Ih_a328 : Inhab a328},
  forall b : bag a328,
  forall b' : bag a328,
  Coq.Init.Logic.iff (subset b b') (
    forall x : a328,
    le (multiplicity x b) (multiplicity x b')
  ).

Parameter filter :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Prop) -> bag a -> bag a.

Axiom filter_mem :
  forall {a335 : Type},
  forall {Ih_a335 : Inhab a335},
  forall b : bag a335,
  forall x : a335,
  forall f : a335 -> bool,
  f x -> eq (multiplicity x (filter f b)) (multiplicity x b).

Axiom filter_mem_neg :
  forall {a342 : Type},
  forall {Ih_a342 : Inhab a342},
  forall b : bag a342,
  forall x : a342,
  forall f : a342 -> bool,
  Coq.Init.Logic.not (f x) -> eq (multiplicity x (filter f b)) (0)%Z.

Parameter cardinal :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> Z.

Parameter finite :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> Prop.

Axiom finite_def :
  forall {a349 : Type},
  forall {Ih_a349 : Inhab a349},
  forall b : bag a349,
  Coq.Init.Logic.iff (finite b) (
    Coq.Init.Logic.ex (
      fun s : sequence a349 =>
      forall x : a349,
      mem x b -> Sequence.mem x s
    )
  ).

Axiom card_nonneg :
  forall {a351 : Type},
  forall {Ih_a351 : Inhab a351},
  forall b : bag a351,
  ge (cardinal b) (0)%Z.

Axiom card_empty :
  forall {a353 : Type},
  forall {Ih_a353 : Inhab a353},
  eq (cardinal (@empty a353 Ih_a353)) (0)%Z.

Axiom card_singleton :
  forall {a357 : Type},
  forall {Ih_a357 : Inhab a357},
  forall x : a357,
  eq (cardinal (singleton x)) (1)%Z.

Axiom card_union :
  forall {a366 : Type},
  forall {Ih_a366 : Inhab a366},
  forall b1 : bag a366,
  forall b2 : bag a366,
  finite b1 ->
  finite b2 ->
  eq (cardinal (union b1 b2)) (plus (cardinal b1) (cardinal b2)).

Axiom card_add :
  forall {a373 : Type},
  forall {Ih_a373 : Inhab a373},
  forall x : a373,
  forall b : bag a373,
  finite b -> eq (cardinal (add x b)) (plus (cardinal b) (1)%Z).

Axiom card_map :
  forall {a380 : Type},
  forall {Ih_a380 : Inhab a380},
  forall f : a380 -> bool,
  forall b : bag a380,
  finite b -> le (cardinal (filter f b)) (cardinal b).

Parameter of_seq :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> bag a.

Axiom of_seq_multiplicity :
  forall {a385 : Type},
  forall {Ih_a385 : Inhab a385},
  forall s : sequence a385,
  forall x : a385,
  eq (Sequence.multiplicity x s) (multiplicity x (of_seq s)).

Parameter fold :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b -> b) -> bag a -> b -> b.

End Bag.

Parameter set_create :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a.

Module _Set.

Parameter t : Type -> Type.

Parameter empty : forall {a : Type}, forall {Ih_a : Inhab a}, set a.

Parameter mem :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> set a -> Prop.

Axiom empty_mem :
  forall {a389 : Type},
  forall {Ih_a389 : Inhab a389},
  forall x : a389,
  Coq.Init.Logic.not (mem x (@empty a389 Ih_a389)).

Parameter add :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> set a -> set a.

Axiom add_mem :
  forall {a393 : Type},
  forall {Ih_a393 : Inhab a393},
  forall s : set a393,
  forall x : a393,
  mem x (add x s).

Axiom add_mem_neq :
  forall {a400 : Type},
  forall {Ih_a400 : Inhab a400},
  forall s : set a400,
  forall x : a400,
  forall y : a400,
  Coq.Init.Logic.not (eq x y) ->
  Coq.Init.Logic.iff (mem x s) (mem x (add y s)).

Parameter singleton :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> set a.

Parameter remove :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> set a -> set a.

Axiom remove_mem :
  forall {a404 : Type},
  forall {Ih_a404 : Inhab a404},
  forall s : set a404,
  forall x : a404,
  Coq.Init.Logic.not (mem x (remove x s)).

Axiom remove_mem_neq :
  forall {a411 : Type},
  forall {Ih_a411 : Inhab a411},
  forall s : set a411,
  forall x : a411,
  forall y : a411,
  Coq.Init.Logic.not (eq x y) ->
  Coq.Init.Logic.iff (mem x s) (mem x (remove y s)).

Parameter union :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> set a -> set a.

Axiom union_mem :
  forall {a418 : Type},
  forall {Ih_a418 : Inhab a418},
  forall s : set a418,
  forall s' : set a418,
  forall x : a418,
  Coq.Init.Logic.or (mem x s) (mem x s') -> mem x (union s s').

Axiom union_mem_neg :
  forall {a425 : Type},
  forall {Ih_a425 : Inhab a425},
  forall s : set a425,
  forall s' : set a425,
  forall x : a425,
  Coq.Init.Logic.not (mem x s) ->
  Coq.Init.Logic.not (mem x s') -> Coq.Init.Logic.not (mem x (union s s')).

Parameter inter :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> set a -> set a.

Axiom inter_mem :
  forall {a432 : Type},
  forall {Ih_a432 : Inhab a432},
  forall s : set a432,
  forall s' : set a432,
  forall x : a432,
  mem x s -> mem x s' -> mem x (inter s s').

Axiom inter_mem_neq :
  forall {a439 : Type},
  forall {Ih_a439 : Inhab a439},
  forall s : set a439,
  forall s' : set a439,
  forall x : a439,
  Coq.Init.Logic.not (Coq.Init.Logic.or (mem x s) (mem x s')) ->
  Coq.Init.Logic.not (mem x (inter s s')).

Parameter disjoint :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> set a -> Prop.

Axiom disjoint_def :
  forall {a443 : Type},
  forall {Ih_a443 : Inhab a443},
  forall s : set a443,
  forall s' : set a443,
  Coq.Init.Logic.iff (disjoint s s') (
    eq (inter s s') (@empty a443 Ih_a443)
  ).

Parameter diff :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> set a -> set a.

Axiom diff_mem :
  forall {a451 : Type},
  forall {Ih_a451 : Inhab a451},
  forall s : set a451,
  forall s' : set a451,
  forall x : a451,
  mem x s' -> Coq.Init.Logic.not (mem x (diff s s')).

Axiom diff_mem_fst :
  forall {a458 : Type},
  forall {Ih_a458 : Inhab a458},
  forall s : set a458,
  forall s' : set a458,
  forall x : a458,
  Coq.Init.Logic.not (mem x s') ->
  Coq.Init.Logic.iff (mem x s) (mem x (diff s s')).

Parameter subset :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> set a -> Prop.

Axiom subset_def :
  forall {a464 : Type},
  forall {Ih_a464 : Inhab a464},
  forall s : set a464,
  forall s' : set a464,
  Coq.Init.Logic.iff (subset s s') (forall x : a464, mem x s -> mem x s').

Parameter map :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b) -> set a -> set b.

Axiom set_map :
  forall {a474 : Type},
  forall {a473 : Type},
  forall {Ih_a474 : Inhab a474},
  forall {Ih_a473 : Inhab a473},
  forall f : a474 -> a473,
  forall s : set a474,
  forall x : a473,
  Coq.Init.Logic.iff (mem x (map f s)) (
    Coq.Init.Logic.ex (
      fun y : a474 =>
      Coq.Init.Logic.and (eq (f y) x) (mem y s)
    )
  ).

Parameter partition :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Prop) -> set a -> set a * set a.

Axiom partition_l_mem :
  forall {a486 : Type},
  forall {Ih_a486 : Inhab a486},
  forall f : a486 -> bool,
  forall s : set a486,
  forall x : a486,
  forall p1 : set a486,
  forall p2 : set a486,
  mem x s -> f x -> eq (partition f s) (p1, p2) -> mem x p1.

Axiom partition_r_mem :
  forall {a498 : Type},
  forall {Ih_a498 : Inhab a498},
  forall f : a498 -> bool,
  forall s : set a498,
  forall x : a498,
  forall p1 : set a498,
  forall p2 : set a498,
  mem x s ->
  Coq.Init.Logic.not (f x) ->
  eq (partition f s) (p1, p2) -> mem x p2.

Parameter cardinal :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> Z.

Parameter finite :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> Prop.

Axiom finite_def :
  forall {a349 : Type},
  forall {Ih_a349 : Inhab a349},
  forall s : set a349,
  Coq.Init.Logic.iff (finite s) (
    Coq.Init.Logic.ex (
      fun seq : sequence a349 =>
      forall x : a349,
      mem x s -> Sequence.mem x seq
    )
  ).

Axiom cardinal_nonneg :
  forall {a504 : Type},
  forall {Ih_a504 : Inhab a504},
  forall s : set a504,
  ge (cardinal s) (0)%Z.

Axiom cardinal_empty :
  forall {a506 : Type},
  forall {Ih_a506 : Inhab a506},
  eq (cardinal (@empty a506 Ih_a506)) (0)%Z.

Axiom cardinal_remove :
  forall {a518 : Type},
  forall {Ih_a518 : Inhab a518},
  forall s : set a518,
  forall x : a518,
  finite s ->
  (
    if classicT (mem x s) then
      eq (cardinal (remove x s)) (minus (cardinal s) (1)%Z)
      else
    eq (cardinal (remove x s)) (cardinal s)
  ).

Axiom cardinal_add :
  forall {a530 : Type},
  forall {Ih_a530 : Inhab a530},
  forall s : set a530,
  forall x : a530,
  finite s ->
  (
    if classicT (mem x s) then
      eq (cardinal (add x s)) (cardinal s)
      else
    eq (cardinal (add x s)) (plus (cardinal s) (1)%Z)
  ).

Parameter of_seq :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> set a.

Parameter fold :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b -> b) -> set a -> b -> b.

End _Set.

Parameter map_set :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b) -> a -> b -> a -> b.

Axiom map_set_def :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  forall f : a -> b,
  forall x : a,
  forall y : b,
  eq (map_set f x y) (
    fun arg : a =>
    if classicT (eq arg x) then y else f x
  ).

Module Map.

Parameter t : Type -> Type -> Type.

Parameter domain :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  b -> (a -> b) -> set a.

Axiom domain_mem :
  forall {a546 : Type},
  forall {a545 : Type},
  forall {Ih_a546 : Inhab a546},
  forall {Ih_a545 : Inhab a545},
  forall x : a546,
  forall m : a546 -> a545,
  forall default : a545,
  Coq.Init.Logic.not (eq (m x) default) -> _Set.mem x (domain default m).

End Map.

Module Array.

Parameter array : Type -> Type.

Parameter get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> Z -> a.

Parameter length :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> Z.

Parameter to_seq :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> sequence a.

Parameter permut :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> array a -> Prop.

Parameter permut_sub :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> array a -> Z -> Z -> Prop.

End Array.

Module List.

Parameter fold_left :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (b -> a -> b) -> b -> list a -> b.

Parameter _exists :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Prop) -> list a -> Prop.

Parameter length :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  list a -> Z.

Parameter nth :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  list a -> Z -> a.

Parameter mem :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> list a -> Prop.

Parameter map :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b) -> list a -> list b.

End List.

Module Order.

Parameter is_pre_order :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> a -> int) -> Prop.

End Order.

Parameter ref : Type -> Type.

Parameter _UNUSED :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  ref a -> a.

Parameter logand : Z -> Z -> Z.

End Stdlib.
