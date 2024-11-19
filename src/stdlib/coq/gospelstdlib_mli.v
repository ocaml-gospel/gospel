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

Parameter succ : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter pred : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter neg : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter plus :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter minus :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter mult :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter div :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter _mod :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter pow :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter abs : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter min :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter max :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

Parameter gt : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

Parameter ge : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

Parameter lt : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

Parameter le : Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

Parameter integer_of_int : int -> Coq.Numbers.BinNums.Z.

Parameter max_int : Coq.Numbers.BinNums.Z.

Parameter min_int : Coq.Numbers.BinNums.Z.

Parameter app :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a -> sequence a.

Parameter seq_get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> a.

Parameter seq_sub :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a ->
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> sequence a.

Parameter seq_sub_l :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> sequence a.

Parameter seq_sub_r :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> sequence a.

Module Sequence.

Parameter t : Type -> Type.

Parameter length :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z.

Parameter in_range :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> Prop.

Axiom in_range_def :
  forall {a4 : Type},
  forall {Ih_a4 : Inhab a4},
  forall s : sequence a4,
  forall i : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.iff (in_range s i) (
    Coq.Init.Logic.and (le (0)%Z i) (lt i (length s))
  ).

Axiom length_nonneg :
  forall {a6 : Type},
  forall {Ih_a6 : Inhab a6},
  forall s : sequence a6,
  le (0)%Z (length s).

Axiom append_length :
  forall {a12 : Type},
  forall {Ih_a12 : Inhab a12},
  forall s : sequence a12,
  forall s' : sequence a12,
  Coq.Init.Logic.eq (length (app s s')) (plus (length s) (length s')).

Axiom append_elems_left :
  forall {a21 : Type},
  forall {Ih_a21 : Inhab a21},
  forall s : sequence a21,
  forall s' : sequence a21,
  forall i : Coq.Numbers.BinNums.Z,
  in_range s i -> Coq.Init.Logic.eq (seq_get (app s s') i) (seq_get s i).

Axiom append_elems_right :
  forall {a32 : Type},
  forall {Ih_a32 : Inhab a32},
  forall s : sequence a32,
  forall s' : sequence a32,
  forall i : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.and (le (length s) i) (
    lt i (plus (length s) (length s'))
  ) ->
  Coq.Init.Logic.eq (seq_get (app s s') i) (
    seq_get s' (minus i (length s))
  ).

Axiom subseq_l :
  forall {a38 : Type},
  forall {Ih_a38 : Inhab a38},
  forall s : sequence a38,
  forall i : Coq.Numbers.BinNums.Z,
  in_range s i ->
  Coq.Init.Logic.eq (seq_sub_l s i) (seq_sub s i (length s)).

Axiom subseq_r :
  forall {a44 : Type},
  forall {Ih_a44 : Inhab a44},
  forall s : sequence a44,
  forall i : Coq.Numbers.BinNums.Z,
  in_range s i -> Coq.Init.Logic.eq (seq_sub_r s i) (seq_sub s (0)%Z i).

Axiom subseq :
  forall {a54 : Type},
  forall {Ih_a54 : Inhab a54},
  forall s : sequence a54,
  forall i : Coq.Numbers.BinNums.Z,
  forall i1 : Coq.Numbers.BinNums.Z,
  forall i2 : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.and (le (0)%Z i1) (
    Coq.Init.Logic.and (le i1 i) (
      Coq.Init.Logic.and (lt i i2) (le i2 (length s))
    )
  ) ->
  Coq.Init.Logic.eq (seq_get s i) (seq_get (seq_sub s i1 i2) (minus i i1)).

Axiom subseq_len :
  forall {a60 : Type},
  forall {Ih_a60 : Inhab a60},
  forall s : sequence a60,
  forall i1 : Coq.Numbers.BinNums.Z,
  forall i2 : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.and (le (0)%Z i1) (
    Coq.Init.Logic.and (le i1 i2) (lt i2 (length s))
  ) ->
  Coq.Init.Logic.eq (length (seq_sub s i1 i2)) (minus i2 i1).

Parameter empty :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a.

Axiom empty_length :
  forall {a63 : Type},
  forall {Ih_a63 : Inhab a63},
  Coq.Init.Logic.eq (length (@empty a63 Ih_a63)) (0)%Z.

Parameter init :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  Coq.Numbers.BinNums.Z -> (Coq.Numbers.BinNums.Z -> a) -> sequence a.

Axiom init_length :
  forall {a68 : Type},
  forall {Ih_a68 : Inhab a68},
  forall n : Coq.Numbers.BinNums.Z,
  forall f : Coq.Numbers.BinNums.Z -> a68,
  ge n (0)%Z -> Coq.Init.Logic.eq (length (init n f)) n.

Axiom init_elems :
  forall {a76 : Type},
  forall {Ih_a76 : Inhab a76},
  forall n : Coq.Numbers.BinNums.Z,
  forall f : Coq.Numbers.BinNums.Z -> a76,
  forall i : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.and (le (0)%Z i) (lt i n) ->
  Coq.Init.Logic.eq (seq_get (init n f) i) (f i).

Parameter singleton :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a.

Axiom singleton_def :
  forall {a82 : Type},
  forall {Ih_a82 : Inhab a82},
  forall x : a82,
  forall f : Coq.Numbers.BinNums.Z -> a82,
  Coq.Init.Logic.eq (f (0)%Z) x ->
  Coq.Init.Logic.eq (singleton x) (init (1)%Z f).

Parameter cons :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a -> sequence a.

Axiom cons_def :
  forall {a88 : Type},
  forall {Ih_a88 : Inhab a88},
  forall x : a88,
  forall s : sequence a88,
  Coq.Init.Logic.eq (cons x s) (app (singleton x) s).

Parameter snoc :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> a -> sequence a.

Axiom snoc_def :
  forall {a94 : Type},
  forall {Ih_a94 : Inhab a94},
  forall s : sequence a94,
  forall x : a94,
  Coq.Init.Logic.eq (snoc s x) (app s (singleton x)).

Parameter hd :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> a.

Axiom hd_def :
  forall {a99 : Type},
  forall {Ih_a99 : Inhab a99},
  forall s : sequence a99,
  Coq.Init.Logic.eq (hd s) (seq_get s (0)%Z).

Parameter tl :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a.

Axiom tl_def :
  forall {a102 : Type},
  forall {Ih_a102 : Inhab a102},
  forall s : sequence a102,
  Coq.Init.Logic.eq (tl s) (seq_sub_l s (1)%Z).

Parameter append :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a -> sequence a.

Axiom append_def :
  forall {a107 : Type},
  forall {Ih_a107 : Inhab a107},
  forall s1 : sequence a107,
  forall s2 : sequence a107,
  Coq.Init.Logic.eq (append s1 s2) (app s1 s2).

Parameter multiplicity :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> sequence a -> Coq.Numbers.BinNums.Z.

Axiom mult_empty :
  forall {a111 : Type},
  forall {Ih_a111 : Inhab a111},
  forall x : a111,
  Coq.Init.Logic.eq (multiplicity x (@empty a111 Ih_a111)) (0)%Z.

Axiom mult_cons :
  forall {a117 : Type},
  forall {Ih_a117 : Inhab a117},
  forall s : sequence a117,
  forall x : a117,
  Coq.Init.Logic.eq (plus (1)%Z (multiplicity x s)) (
    multiplicity x (cons x s)
  ).

Axiom mult_cons_neutral :
  forall {a125 : Type},
  forall {Ih_a125 : Inhab a125},
  forall s : sequence a125,
  forall x1 : a125,
  forall x2 : a125,
  Coq.Init.Logic.not (Coq.Init.Logic.eq x1 x2) ->
  Coq.Init.Logic.eq (multiplicity x1 s) (multiplicity x1 (cons x2 s)).

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
  forall i : Coq.Numbers.BinNums.Z,
  forall f : a142 -> a144,
  forall s : sequence a142,
  Coq.Init.Logic.and (le (0)%Z i) (lt i (length s)) ->
  Coq.Init.Logic.eq (seq_get (map f s) i) (f (seq_get s i)).

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

Parameter get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> a.

Axiom get_def :
  forall {a156 : Type},
  forall {Ih_a156 : Inhab a156},
  forall s : sequence a156,
  forall i : Coq.Numbers.BinNums.Z,
  Coq.Init.Logic.eq (get s i) (seq_get s i).

Parameter set :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> Coq.Numbers.BinNums.Z -> a -> sequence a.

Axiom set_elem :
  forall {a163 : Type},
  forall {Ih_a163 : Inhab a163},
  forall s : sequence a163,
  forall i : Coq.Numbers.BinNums.Z,
  forall x : a163,
  Coq.Init.Logic.and (le (0)%Z i) (lt i (length s)) ->
  Coq.Init.Logic.eq (seq_get (set s i x) i) x.

Axiom set_elem_other :
  forall {a174 : Type},
  forall {Ih_a174 : Inhab a174},
  forall s : sequence a174,
  forall i1 : Coq.Numbers.BinNums.Z,
  forall i2 : Coq.Numbers.BinNums.Z,
  forall x : a174,
  Coq.Init.Logic.not (Coq.Init.Logic.eq i1 i2) ->
  Coq.Init.Logic.and (le (0)%Z i1) (lt i1 (length s)) ->
  Coq.Init.Logic.and (le (0)%Z i2) (lt i2 (length s)) ->
  Coq.Init.Logic.eq (seq_get (set s i1 x) i2) (seq_get s i2).

Parameter rev :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  sequence a -> sequence a.

Axiom rev_length :
  forall {a178 : Type},
  forall {Ih_a178 : Inhab a178},
  forall s : sequence a178,
  Coq.Init.Logic.eq (length s) (length (rev s)).

Axiom rev_elems :
  forall {a187 : Type},
  forall {Ih_a187 : Inhab a187},
  forall i : Coq.Numbers.BinNums.Z,
  forall s : sequence a187,
  Coq.Init.Logic.and (le (0)%Z i) (lt i (length s)) ->
  Coq.Init.Logic.eq (seq_get (rev s) i) (
    seq_get s (minus (minus (length s) (1)%Z) i)
  ).

Axiom extensionality :
  forall {a197 : Type},
  forall {Ih_a197 : Inhab a197},
  forall s1 : sequence a197,
  forall s2 : sequence a197,
  Coq.Init.Logic.eq (length s1) (length s2) ->
  (
    forall i : Coq.Numbers.BinNums.Z,
    Coq.Init.Logic.and (le (0)%Z i) (lt i (length s1)) ->
    Coq.Init.Logic.eq (seq_get s1 i) (seq_get s2 i)
  ) ->
  Coq.Init.Logic.eq s1 s2.

Parameter fold_left :
  forall {a : Type},
  forall {b : Type},
  forall {Ih_a : Inhab a},
  forall {Ih_b : Inhab b},
  (a -> b -> a) -> a -> sequence b -> a.

Axiom fold_left_empty :
  forall {a203 : Type},
  forall {a204 : Type},
  forall {Ih_a203 : Inhab a203},
  forall {Ih_a204 : Inhab a204},
  forall f : a204 -> a203 -> a204,
  forall acc : a204,
  Coq.Init.Logic.eq (fold_left f acc (@empty a203 Ih_a203)) acc.

Axiom fold_left_cons :
  forall {a215 : Type},
  forall {a216 : Type},
  forall {Ih_a215 : Inhab a215},
  forall {Ih_a216 : Inhab a216},
  forall f : a216 -> a215 -> a216,
  forall acc : a216,
  forall x : a215,
  forall l : sequence a215,
  Coq.Init.Logic.eq (fold_left f acc (cons x l)) (fold_left f (f acc x) l).

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
  Coq.Init.Logic.eq (fold_right f (@empty a221 Ih_a221) acc) acc.

Axiom fold_right_cons :
  forall {a232 : Type},
  forall {a234 : Type},
  forall {Ih_a232 : Inhab a232},
  forall {Ih_a234 : Inhab a234},
  forall acc : a234,
  forall f : a232 -> a234 -> a234,
  forall x : a232,
  forall l : sequence a232,
  Coq.Init.Logic.eq (fold_right f (cons x l) acc) (
    f x (fold_right f l acc)
  ).

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
  a -> bag a -> Coq.Numbers.BinNums.Z.

Axiom well_formed :
  forall {a237 : Type},
  forall {Ih_a237 : Inhab a237},
  forall x : a237,
  forall b : bag a237,
  ge (multiplicity x b) (0)%Z.

Parameter empty : forall {a : Type}, forall {Ih_a : Inhab a}, bag a.

Axiom empty_mult :
  forall {a240 : Type},
  forall {Ih_a240 : Inhab a240},
  forall x : a240,
  Coq.Init.Logic.eq (multiplicity x (@empty a240 Ih_a240)) (0)%Z.

Parameter init :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Coq.Numbers.BinNums.Z) -> bag a.

Axiom init_axiom :
  forall {a246 : Type},
  forall {Ih_a246 : Inhab a246},
  forall f : a246 -> Coq.Numbers.BinNums.Z,
  forall x : a246,
  Coq.Init.Logic.eq (max (0)%Z (f x)) (multiplicity x (init f)).

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
  Coq.Init.Logic.eq (multiplicity x (add x b)) (
    plus (1)%Z (multiplicity x b)
  ).

Axiom add_mult_neg_x :
  forall {a264 : Type},
  forall {Ih_a264 : Inhab a264},
  forall x : a264,
  forall y : a264,
  forall b : bag a264,
  Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
  Coq.Init.Logic.eq (multiplicity y (add x b)) (multiplicity y b).

Parameter singleton :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a.

Axiom singleton_def :
  forall {a269 : Type},
  forall {Ih_a269 : Inhab a269},
  forall x : a269,
  Coq.Init.Logic.eq (singleton x) (add x (@empty a269 Ih_a269)).

Parameter remove :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  a -> bag a -> bag a.

Axiom remove_mult_x :
  forall {a275 : Type},
  forall {Ih_a275 : Inhab a275},
  forall b : bag a275,
  forall x : a275,
  Coq.Init.Logic.eq (multiplicity x (remove x b)) (
    max (0)%Z (minus (multiplicity x b) (1)%Z)
  ).

Axiom remove_mult_neg_x :
  forall {a283 : Type},
  forall {Ih_a283 : Inhab a283},
  forall x : a283,
  forall y : a283,
  forall b : bag a283,
  Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
  Coq.Init.Logic.eq (multiplicity y (remove x b)) (multiplicity y b).

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
  Coq.Init.Logic.eq (max (multiplicity x b) (multiplicity x b')) (
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
  Coq.Init.Logic.eq (plus (multiplicity x b) (multiplicity x b')) (
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
  Coq.Init.Logic.eq (min (multiplicity x b) (multiplicity x b')) (
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
  Coq.Init.Logic.eq (
    max (0)%Z (minus (multiplicity x b) (multiplicity x b'))
  ) (multiplicity x (diff b b')).

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
  f x ->
  Coq.Init.Logic.eq (multiplicity x (filter f b)) (multiplicity x b).

Axiom filter_mem_neg :
  forall {a342 : Type},
  forall {Ih_a342 : Inhab a342},
  forall b : bag a342,
  forall x : a342,
  forall f : a342 -> bool,
  Coq.Init.Logic.not (f x) ->
  Coq.Init.Logic.eq (multiplicity x (filter f b)) (0)%Z.

Parameter cardinal :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  bag a -> Coq.Numbers.BinNums.Z.

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
  Coq.Init.Logic.eq (cardinal (@empty a353 Ih_a353)) (0)%Z.

Axiom card_singleton :
  forall {a357 : Type},
  forall {Ih_a357 : Inhab a357},
  forall x : a357,
  Coq.Init.Logic.eq (cardinal (singleton x)) (1)%Z.

Axiom card_union :
  forall {a366 : Type},
  forall {Ih_a366 : Inhab a366},
  forall b1 : bag a366,
  forall b2 : bag a366,
  finite b1 ->
  finite b2 ->
  Coq.Init.Logic.eq (cardinal (union b1 b2)) (
    plus (cardinal b1) (cardinal b2)
  ).

Axiom card_add :
  forall {a373 : Type},
  forall {Ih_a373 : Inhab a373},
  forall x : a373,
  forall b : bag a373,
  finite b ->
  Coq.Init.Logic.eq (cardinal (add x b)) (plus (cardinal b) (1)%Z).

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
  Coq.Init.Logic.eq (Sequence.multiplicity x s) (
    multiplicity x (of_seq s)
  ).

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
  Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
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
  Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
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
    Coq.Init.Logic.eq (inter s s') (@empty a443 Ih_a443)
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
  forall {a473 : Type},
  forall {a474 : Type},
  forall {Ih_a473 : Inhab a473},
  forall {Ih_a474 : Inhab a474},
  forall f : a474 -> a473,
  forall s : set a474,
  forall x : a473,
  Coq.Init.Logic.iff (mem x (map f s)) (
    Coq.Init.Logic.ex (
      fun y : a474 =>
      Coq.Init.Logic.and (Coq.Init.Logic.eq (f y) x) (mem y s)
    )
  ).

Parameter partition :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  (a -> Prop) -> set a -> (set a) * (set a).

Axiom partition_l_mem :
  forall {a486 : Type},
  forall {Ih_a486 : Inhab a486},
  forall f : a486 -> bool,
  forall s : set a486,
  forall x : a486,
  forall p1 : set a486,
  forall p2 : set a486,
  mem x s ->
  f x -> Coq.Init.Logic.eq (partition f s) (p1, p2) -> mem x p1.

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
  Coq.Init.Logic.eq (partition f s) (p1, p2) -> mem x p2.

Parameter cardinal :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> Coq.Numbers.BinNums.Z.

Parameter finite :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  set a -> Prop.

Axiom finite_def :
  forall {a504 : Type},
  forall {Ih_a504 : Inhab a504},
  forall s : set a504,
  Coq.Init.Logic.iff (finite s) (
    Coq.Init.Logic.ex (
      fun seq : sequence a504 =>
      forall x : a504,
      mem x s -> Sequence.mem x seq
    )
  ).

Axiom cardinal_nonneg :
  forall {a506 : Type},
  forall {Ih_a506 : Inhab a506},
  forall s : set a506,
  ge (cardinal s) (0)%Z.

Axiom cardinal_empty :
  forall {a508 : Type},
  forall {Ih_a508 : Inhab a508},
  Coq.Init.Logic.eq (cardinal (@empty a508 Ih_a508)) (0)%Z.

Axiom cardinal_remove :
  forall {a520 : Type},
  forall {Ih_a520 : Inhab a520},
  forall s : set a520,
  forall x : a520,
  finite s ->
  (
    if classicT (mem x s) then
      Coq.Init.Logic.eq (cardinal (remove x s)) (minus (cardinal s) (1)%Z)
      else
    Coq.Init.Logic.eq (cardinal (remove x s)) (cardinal s)
  ).

Axiom cardinal_add :
  forall {a532 : Type},
  forall {Ih_a532 : Inhab a532},
  forall s : set a532,
  forall x : a532,
  finite s ->
  (
    if classicT (mem x s) then
      Coq.Init.Logic.eq (cardinal (add x s)) (cardinal s)
      else
    Coq.Init.Logic.eq (cardinal (add x s)) (plus (cardinal s) (1)%Z)
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
  Coq.Init.Logic.eq (map_set f x y) (
    fun arg : a =>
    if classicT (Coq.Init.Logic.eq arg x) then y else f x
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
  forall {a547 : Type},
  forall {a548 : Type},
  forall {Ih_a547 : Inhab a547},
  forall {Ih_a548 : Inhab a548},
  forall x : a548,
  forall m : a548 -> a547,
  forall default : a547,
  Coq.Init.Logic.not (Coq.Init.Logic.eq (m x) default) ->
  _Set.mem x (domain default m).

End Map.

Module Array.

Parameter array : Type -> Type.
Parameter get :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> Coq.Numbers.BinNums.Z -> a.

Parameter length :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  array a -> Coq.Numbers.BinNums.Z.

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
  array a ->
  array a -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

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
  list a -> Coq.Numbers.BinNums.Z.

Parameter nth :
  forall {a : Type},
  forall {Ih_a : Inhab a},
  list a -> Coq.Numbers.BinNums.Z -> a.

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

Parameter logand :
  Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

End Stdlib.


