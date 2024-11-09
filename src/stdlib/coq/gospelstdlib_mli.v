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

    Parameter integer_of_int : int -> Z.

    Parameter of_list :
      forall {a : Type},
      forall {Ih_a : Inhab a},
        list a -> sequence a.

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

    Parameter monoid :
      forall {a : Type},
      forall {Ih_a : Inhab a},
        (a -> a -> a) -> a -> Prop.

    Axiom monoid_def :
      forall {a59 : Type},
      forall {Ih_a59 : Inhab a59},
      forall f : a59 -> a59 -> a59,
      forall neutral : a59,
        Coq.Init.Logic.iff (monoid f neutral) (
            Coq.Init.Logic.and (
                forall x : a59,
                  Coq.Init.Logic.and (eq (f neutral x) (f x neutral)) (eq (f x neutral) x)
              ) (
                forall x : a59,
                forall y : a59,
                forall z : a59,
                  eq (f x (f y z)) (f (f x y) z)
              )
          ).

    Parameter comm_monoid :
      forall {a : Type},
      forall {Ih_a : Inhab a},
        (a -> a -> a) -> a -> Prop.

    Axiom comm_monoid_def :
      forall {a70 : Type},
      forall {Ih_a70 : Inhab a70},
      forall f : a70 -> a70 -> a70,
      forall neutral : a70,
        Coq.Init.Logic.iff (comm_monoid f neutral) (
            Coq.Init.Logic.and (monoid f neutral) (
                forall x : a70,
                forall y : a70,
                  eq (f x y) (f y x)
              )
          ).


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
        forall {a41 : Type},
        forall {Ih_a41 : Inhab a41},
        forall s : sequence a41,
        forall i : Z,
          Coq.Init.Logic.iff (in_range s i) (
              Coq.Init.Logic.and (le (0)%Z i) (lt i (length s))
            ).

      Axiom length_nonneg :
        forall {a43 : Type},
        forall {Ih_a43 : Inhab a43},
        forall s : sequence a43,
          le (0)%Z (length s).

      Axiom subseq_l :
        forall {a49 : Type},
        forall {Ih_a49 : Inhab a49},
        forall s : sequence a49,
        forall i : Z,
          in_range s i -> eq (seq_sub_l s i) (seq_sub s i (length s)).

      Axiom subseq_r :
        forall {a55 : Type},
        forall {Ih_a55 : Inhab a55},
        forall s : sequence a55,
        forall i : Z,
          in_range s i -> eq (seq_sub_r s i) (seq_sub s (0)%Z i).

      Axiom subseq :
        forall {a65 : Type},
        forall {Ih_a65 : Inhab a65},
        forall s : sequence a65,
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
        forall {a71 : Type},
        forall {Ih_a71 : Inhab a71},
        forall s : sequence a71,
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
        forall {a74 : Type},
        forall {Ih_a74 : Inhab a74},
          eq (length (@empty a74 Ih_a74)) (0)%Z.

      Parameter init :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          Z -> (Z -> a) -> sequence a.

      Axiom init_length :
        forall {a79 : Type},
        forall {Ih_a79 : Inhab a79},
        forall n : Z,
        forall f : Z -> a79,
          ge n (0)%Z -> eq (length (init n f)) n.

      Axiom init_elems :
        forall {a87 : Type},
        forall {Ih_a87 : Inhab a87},
        forall n : Z,
        forall f : Z -> a87,
        forall i : Z,
          Coq.Init.Logic.and (le (0)%Z i) (lt i n) ->
          eq (seq_get (init n f) i) (f i).

      Parameter singleton :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> sequence a.

      Axiom singleton_def :
        forall {a93 : Type},
        forall {Ih_a93 : Inhab a93},
        forall x : a93,
        forall f : Z -> a93,
          eq (f (0)%Z) x -> eq (singleton x) (init (1)%Z f).

      Parameter cons :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> sequence a -> sequence a.

      Axiom cons_def :
        forall {a99 : Type},
        forall {Ih_a99 : Inhab a99},
        forall x : a99,
        forall s : sequence a99,
          eq (cons x s) (app (singleton x) s).

      Parameter snoc :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> a -> sequence a.

      Axiom snoc_def :
        forall {a105 : Type},
        forall {Ih_a105 : Inhab a105},
        forall s : sequence a105,
        forall x : a105,
          eq (snoc s x) (app s (singleton x)).

      Parameter hd :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> a.

      Axiom hd_def :
        forall {a110 : Type},
        forall {Ih_a110 : Inhab a110},
        forall s : sequence a110,
          eq (hd s) (seq_get s (0)%Z).

      Parameter tl :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> sequence a.

      Axiom tl_def :
        forall {a113 : Type},
        forall {Ih_a113 : Inhab a113},
        forall s : sequence a113,
          eq (tl s) (seq_sub_l s (1)%Z).

      Parameter append :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> sequence a -> sequence a.

      Axiom append_def :
        forall {a118 : Type},
        forall {Ih_a118 : Inhab a118},
        forall s1 : sequence a118,
        forall s2 : sequence a118,
          eq (append s1 s2) (app s1 s2).

      Axiom append_length :
        forall {a125 : Type},
        forall {Ih_a125 : Inhab a125},
        forall s : sequence a125,
        forall s' : sequence a125,
          eq (length (app s s')) (plus (length s) (length s')).

      Axiom append_elems_left :
        forall {a134 : Type},
        forall {Ih_a134 : Inhab a134},
        forall s : sequence a134,
        forall s' : sequence a134,
        forall i : Z,
          in_range s i -> eq (seq_get (app s s') i) (seq_get s i).

      Axiom append_elems_right :
        forall {a145 : Type},
        forall {Ih_a145 : Inhab a145},
        forall s : sequence a145,
        forall s' : sequence a145,
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
        forall {a148 : Type},
        forall {Ih_a148 : Inhab a148},
        forall x : a148,
          eq (multiplicity x (@empty a148 Ih_a148)) (0)%Z.

      Axiom mult_cons :
        forall {a154 : Type},
        forall {Ih_a154 : Inhab a154},
        forall s : sequence a154,
        forall x : a154,
          eq (plus (1)%Z (multiplicity x s)) (multiplicity x (cons x s)).

      Axiom mult_cons_neutral :
        forall {a162 : Type},
        forall {Ih_a162 : Inhab a162},
        forall s : sequence a162,
        forall x1 : a162,
        forall x2 : a162,
          Coq.Init.Logic.not (eq x1 x2) ->
          eq (multiplicity x1 s) (multiplicity x1 (cons x2 s)).

      Axiom mult_length :
        forall {a167 : Type},
        forall {Ih_a167 : Inhab a167},
        forall x : a167,
        forall s : sequence a167,
          Coq.Init.Logic.and (le (0)%Z (multiplicity x s)) (
              le (multiplicity x s) (length s)
            ).

      Parameter mem :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> sequence a -> Prop.

      Axiom mem_def :
        forall {a171 : Type},
        forall {Ih_a171 : Inhab a171},
        forall s : sequence a171,
        forall x : a171,
          Coq.Init.Logic.iff (mem x s) (gt (multiplicity x s) (0)%Z).

      Parameter _forall :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Prop) -> sequence a -> Prop.

      Axiom forall_def :
        forall {a176 : Type},
        forall {Ih_a176 : Inhab a176},
        forall p : a176 -> bool,
        forall s : sequence a176,
          Coq.Init.Logic.iff (_forall p s) (forall x : a176, mem x s -> p x).

      Parameter _exists :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Prop) -> sequence a -> Prop.

      Axiom _exists_def :
        forall {a182 : Type},
        forall {Ih_a182 : Inhab a182},
        forall p : a182 -> bool,
        forall s : sequence a182,
          Coq.Init.Logic.iff (_exists p s) (
              Coq.Init.Logic.ex (fun x : a182 => Coq.Init.Logic.and (mem x s) (p x))
            ).

      Parameter map :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b) -> sequence a -> sequence b.

      Axiom map_elems :
        forall {a191 : Type},
        forall {a193 : Type},
        forall {Ih_a191 : Inhab a191},
        forall {Ih_a193 : Inhab a193},
        forall i : Z,
        forall f : a191 -> a193,
        forall s : sequence a191,
          in_range s i -> eq (seq_get (map f s) i) (f (seq_get s i)).

      Parameter filter :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Prop) -> sequence a -> sequence a.

      Axiom filter_elems :
        forall {a200 : Type},
        forall {Ih_a200 : Inhab a200},
        forall f : a200 -> bool,
        forall s : sequence a200,
        forall x : a200,
          mem x s -> f x -> mem x (filter f s).

      Parameter filter_map :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> option b) -> sequence a -> sequence b.

      Axiom filter_map_elems :
        forall {a210 : Type},
        forall {a211 : Type},
        forall {Ih_a210 : Inhab a210},
        forall {Ih_a211 : Inhab a211},
        forall f : a210 -> option a211,
        forall s : sequence a210,
        forall y : a211,
          Coq.Init.Logic.iff (
              Coq.Init.Logic.ex (
                  fun x : a210 =>
                    Coq.Init.Logic.and (eq (f x) (Some y)) (mem x s)
                )
            ) (mem y (filter_map f s)).

      Parameter get :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> Z -> a.

      Axiom get_def :
        forall {a216 : Type},
        forall {Ih_a216 : Inhab a216},
        forall s : sequence a216,
        forall i : Z,
          eq (get s i) (seq_get s i).

      Parameter set :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> Z -> a -> sequence a.

      Axiom set_elem :
        forall {a223 : Type},
        forall {Ih_a223 : Inhab a223},
        forall s : sequence a223,
        forall i : Z,
        forall x : a223,
          in_range s i -> eq (seq_get (set s i x) i) x.

      Axiom set_elem_other :
        forall {a234 : Type},
        forall {Ih_a234 : Inhab a234},
        forall s : sequence a234,
        forall i1 : Z,
        forall i2 : Z,
        forall x : a234,
          Coq.Init.Logic.not (eq i1 i2) ->
          in_range s i1 ->
          in_range s i2 -> eq (seq_get (set s i1 x) i2) (seq_get s i2).

      Parameter rev :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> sequence a.

      Axiom rev_length :
        forall {a238 : Type},
        forall {Ih_a238 : Inhab a238},
        forall s : sequence a238,
          eq (length s) (length (rev s)).

      Axiom rev_elems :
        forall {a247 : Type},
        forall {Ih_a247 : Inhab a247},
        forall i : Z,
        forall s : sequence a247,
          in_range s i ->
          eq (seq_get (rev s) i) (seq_get s (minus (minus (length s) (1)%Z) i)).

      Axiom extensionality :
        forall {a257 : Type},
        forall {Ih_a257 : Inhab a257},
        forall s1 : sequence a257,
        forall s2 : sequence a257,
          eq (length s1) (length s2) ->
          (forall i : Z, in_range s1 i -> eq (seq_get s1 i) (seq_get s2 i)) ->
          eq s1 s2.

      Parameter fold_left :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b -> a) -> a -> sequence b -> a.

      Axiom fold_left_empty :
        forall {a263 : Type},
        forall {a264 : Type},
        forall {Ih_a263 : Inhab a263},
        forall {Ih_a264 : Inhab a264},
        forall f : a264 -> a263 -> a264,
        forall acc : a264,
          eq (fold_left f acc (@empty a263 Ih_a263)) acc.

      Axiom fold_left_cons :
        forall {a275 : Type},
        forall {a276 : Type},
        forall {Ih_a275 : Inhab a275},
        forall {Ih_a276 : Inhab a276},
        forall f : a276 -> a275 -> a276,
        forall acc : a276,
        forall x : a275,
        forall l : sequence a275,
          eq (fold_left f acc (cons x l)) (fold_left f (f acc x) l).

      Parameter fold_right :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b -> b) -> sequence a -> b -> b.

      Axiom fold_right_empty :
        forall {a281 : Type},
        forall {a282 : Type},
        forall {Ih_a281 : Inhab a281},
        forall {Ih_a282 : Inhab a282},
        forall acc : a282,
        forall f : a281 -> a282 -> a282,
          eq (fold_right f (@empty a281 Ih_a281) acc) acc.

      Axiom fold_right_cons :
        forall {a292 : Type},
        forall {a294 : Type},
        forall {Ih_a292 : Inhab a292},
        forall {Ih_a294 : Inhab a294},
        forall acc : a294,
        forall f : a292 -> a294 -> a294,
        forall x : a292,
        forall l : sequence a292,
          eq (fold_right f (cons x l) acc) (f x (fold_right f l acc)).

      Parameter permut :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> sequence a -> Prop.

      Axiom permut_mem :
        forall {a300 : Type},
        forall {Ih_a300 : Inhab a300},
        forall s1 : sequence a300,
        forall s2 : sequence a300,
          permut s1 s2 ->
          (forall x : a300, Coq.Init.Logic.iff (mem x s1) (mem x s2)).

      Parameter permut_sub :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> sequence a -> Z -> Z -> Prop.

      Axiom permut_sub_def :
        forall {a314 : Type},
        forall {Ih_a314 : Inhab a314},
        forall s1 : sequence a314,
        forall s2 : sequence a314,
        forall i : Z,
        forall j : Z,
          permut (seq_sub s1 i j) (seq_sub s2 i j) ->
          eq (seq_sub_r s1 i) (seq_sub_r s2 i) ->
          eq (seq_sub_l s1 j) (seq_sub_l s2 j) -> permut_sub s1 s2 i j.

    End Sequence.

    Module Bag.

      Parameter t : Type -> Type.

      Parameter multiplicity :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> bag a -> Z.

      Axiom well_formed :
        forall {a317 : Type},
        forall {Ih_a317 : Inhab a317},
        forall b : bag a317,
        forall x : a317,
          ge (multiplicity x b) (0)%Z.

      Parameter empty : forall {a : Type}, forall {Ih_a : Inhab a}, bag a.

      Axiom empty_mult :
        forall {a320 : Type},
        forall {Ih_a320 : Inhab a320},
        forall x : a320,
          eq (multiplicity x (@empty a320 Ih_a320)) (0)%Z.

      Parameter init :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Z) -> bag a.

      Axiom init_axiom :
        forall {a326 : Type},
        forall {Ih_a326 : Inhab a326},
        forall f : a326 -> Z,
        forall x : a326,
          eq (max (0)%Z (f x)) (multiplicity x (init f)).

      Parameter mem :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> bag a -> Prop.

      Axiom mem_def :
        forall {a331 : Type},
        forall {Ih_a331 : Inhab a331},
        forall x : a331,
        forall b : bag a331,
          Coq.Init.Logic.iff (mem x b) (gt (multiplicity x b) (0)%Z).

      Parameter add :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> bag a -> bag a.

      Axiom add_mult_x :
        forall {a336 : Type},
        forall {Ih_a336 : Inhab a336},
        forall b : bag a336,
        forall x : a336,
          eq (multiplicity x (add x b)) (plus (1)%Z (multiplicity x b)).

      Axiom add_mult_neg_x :
        forall {a344 : Type},
        forall {Ih_a344 : Inhab a344},
        forall x : a344,
        forall y : a344,
        forall b : bag a344,
          Coq.Init.Logic.not (eq x y) ->
          eq (multiplicity y (add x b)) (multiplicity y b).

      Parameter singleton :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> bag a.

      Axiom singleton_def :
        forall {a349 : Type},
        forall {Ih_a349 : Inhab a349},
        forall x : a349,
          eq (singleton x) (add x (@empty a349 Ih_a349)).

      Parameter remove :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> bag a -> bag a.

      Axiom remove_mult_x :
        forall {a355 : Type},
        forall {Ih_a355 : Inhab a355},
        forall b : bag a355,
        forall x : a355,
          eq (multiplicity x (remove x b)) (
              max (0)%Z (minus (multiplicity x b) (1)%Z)
            ).

      Axiom remove_mult_neg_x :
        forall {a363 : Type},
        forall {Ih_a363 : Inhab a363},
        forall x : a363,
        forall y : a363,
        forall b : bag a363,
          Coq.Init.Logic.not (eq x y) ->
          eq (multiplicity y (remove x b)) (multiplicity y b).

      Parameter union :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> bag a.

      Axiom union_all :
        forall {a371 : Type},
        forall {Ih_a371 : Inhab a371},
        forall b : bag a371,
        forall b' : bag a371,
        forall x : a371,
          eq (max (multiplicity x b) (multiplicity x b')) (
              multiplicity x (union b b')
            ).

      Parameter sum :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> bag a.

      Axiom sum_all :
        forall {a379 : Type},
        forall {Ih_a379 : Inhab a379},
        forall b : bag a379,
        forall b' : bag a379,
        forall x : a379,
          eq (plus (multiplicity x b) (multiplicity x b')) (
              multiplicity x (sum b b')
            ).

      Parameter inter :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> bag a.

      Axiom inter_all :
        forall {a387 : Type},
        forall {Ih_a387 : Inhab a387},
        forall b : bag a387,
        forall b' : bag a387,
        forall x : a387,
          eq (min (multiplicity x b) (multiplicity x b')) (
              multiplicity x (inter b b')
            ).

      Parameter disjoint :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> Prop.

      Axiom disjoint_def :
        forall {a394 : Type},
        forall {Ih_a394 : Inhab a394},
        forall b : bag a394,
        forall b' : bag a394,
          Coq.Init.Logic.iff (disjoint b b') (
              forall x : a394,
                mem x b -> Coq.Init.Logic.not (mem x b')
            ).

      Parameter diff :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> bag a.

      Axiom diff_all :
        forall {a401 : Type},
        forall {Ih_a401 : Inhab a401},
        forall b : bag a401,
        forall b' : bag a401,
        forall x : a401,
          eq (max (0)%Z (minus (multiplicity x b) (multiplicity x b'))) (
              multiplicity x (diff b b')
            ).

      Parameter subset :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          bag a -> bag a -> Prop.

      Axiom subset_def :
        forall {a408 : Type},
        forall {Ih_a408 : Inhab a408},
        forall b : bag a408,
        forall b' : bag a408,
          Coq.Init.Logic.iff (subset b b') (
              forall x : a408,
                le (multiplicity x b) (multiplicity x b')
            ).

      Parameter filter :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Prop) -> bag a -> bag a.

      Axiom filter_mem :
        forall {a415 : Type},
        forall {Ih_a415 : Inhab a415},
        forall b : bag a415,
        forall x : a415,
        forall f : a415 -> bool,
          f x -> eq (multiplicity x (filter f b)) (multiplicity x b).

      Axiom filter_mem_neg :
        forall {a422 : Type},
        forall {Ih_a422 : Inhab a422},
        forall b : bag a422,
        forall x : a422,
        forall f : a422 -> bool,
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
        forall {a429 : Type},
        forall {Ih_a429 : Inhab a429},
        forall b : bag a429,
          Coq.Init.Logic.iff (finite b) (
              Coq.Init.Logic.ex (
                  fun s : sequence a429 =>
                    forall x : a429,
                      mem x b -> Sequence.mem x s
                )
            ).

      Axiom card_nonneg :
        forall {a431 : Type},
        forall {Ih_a431 : Inhab a431},
        forall b : bag a431,
          ge (cardinal b) (0)%Z.

      Axiom card_empty :
        forall {a433 : Type},
        forall {Ih_a433 : Inhab a433},
          eq (cardinal (@empty a433 Ih_a433)) (0)%Z.

      Axiom card_singleton :
        forall {a437 : Type},
        forall {Ih_a437 : Inhab a437},
        forall x : a437,
          eq (cardinal (singleton x)) (1)%Z.

      Axiom card_union :
        forall {a446 : Type},
        forall {Ih_a446 : Inhab a446},
        forall b1 : bag a446,
        forall b2 : bag a446,
          finite b1 ->
          finite b2 ->
          eq (cardinal (union b1 b2)) (plus (cardinal b1) (cardinal b2)).

      Axiom card_add :
        forall {a453 : Type},
        forall {Ih_a453 : Inhab a453},
        forall x : a453,
        forall b : bag a453,
          finite b -> eq (cardinal (add x b)) (plus (cardinal b) (1)%Z).

      Axiom card_map :
        forall {a460 : Type},
        forall {Ih_a460 : Inhab a460},
        forall f : a460 -> bool,
        forall b : bag a460,
          finite b -> le (cardinal (filter f b)) (cardinal b).

      Parameter of_seq :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          sequence a -> bag a.

      Axiom of_seq_multiplicity :
        forall {a465 : Type},
        forall {Ih_a465 : Inhab a465},
        forall s : sequence a465,
        forall x : a465,
          eq (Sequence.multiplicity x s) (multiplicity x (of_seq s)).

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
        forall {a469 : Type},
        forall {Ih_a469 : Inhab a469},
        forall x : a469,
          Coq.Init.Logic.not (mem x (@empty a469 Ih_a469)).

      Parameter add :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          a -> set a -> set a.

      Axiom add_mem :
        forall {a473 : Type},
        forall {Ih_a473 : Inhab a473},
        forall s : set a473,
        forall x : a473,
          mem x (add x s).

      Axiom add_mem_neq :
        forall {a480 : Type},
        forall {Ih_a480 : Inhab a480},
        forall s : set a480,
        forall x : a480,
        forall y : a480,
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
        forall {a484 : Type},
        forall {Ih_a484 : Inhab a484},
        forall s : set a484,
        forall x : a484,
          Coq.Init.Logic.not (mem x (remove x s)).

      Axiom remove_mem_neq :
        forall {a491 : Type},
        forall {Ih_a491 : Inhab a491},
        forall s : set a491,
        forall x : a491,
        forall y : a491,
          Coq.Init.Logic.not (eq x y) ->
          Coq.Init.Logic.iff (mem x s) (mem x (remove y s)).

      Parameter union :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> set a -> set a.

      Axiom union_mem :
        forall {a498 : Type},
        forall {Ih_a498 : Inhab a498},
        forall s : set a498,
        forall s' : set a498,
        forall x : a498,
          Coq.Init.Logic.or (mem x s) (mem x s') -> mem x (union s s').

      Axiom union_mem_neg :
        forall {a505 : Type},
        forall {Ih_a505 : Inhab a505},
        forall s : set a505,
        forall s' : set a505,
        forall x : a505,
          Coq.Init.Logic.not (mem x s) ->
          Coq.Init.Logic.not (mem x s') -> Coq.Init.Logic.not (mem x (union s s')).

      Parameter inter :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> set a -> set a.

      Axiom inter_mem :
        forall {a512 : Type},
        forall {Ih_a512 : Inhab a512},
        forall s : set a512,
        forall s' : set a512,
        forall x : a512,
          mem x s -> mem x s' -> mem x (inter s s').

      Axiom inter_mem_neq :
        forall {a519 : Type},
        forall {Ih_a519 : Inhab a519},
        forall s : set a519,
        forall s' : set a519,
        forall x : a519,
          Coq.Init.Logic.not (Coq.Init.Logic.or (mem x s) (mem x s')) ->
          Coq.Init.Logic.not (mem x (inter s s')).

      Parameter disjoint :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> set a -> Prop.

      Axiom disjoint_def :
        forall {a523 : Type},
        forall {Ih_a523 : Inhab a523},
        forall s : set a523,
        forall s' : set a523,
          Coq.Init.Logic.iff (disjoint s s') (
              eq (inter s s') (@empty a523 Ih_a523)
            ).

      Parameter diff :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> set a -> set a.

      Axiom diff_mem :
        forall {a531 : Type},
        forall {Ih_a531 : Inhab a531},
        forall s : set a531,
        forall s' : set a531,
        forall x : a531,
          mem x s' -> Coq.Init.Logic.not (mem x (diff s s')).

      Axiom diff_mem_fst :
        forall {a538 : Type},
        forall {Ih_a538 : Inhab a538},
        forall s : set a538,
        forall s' : set a538,
        forall x : a538,
          Coq.Init.Logic.not (mem x s') ->
          Coq.Init.Logic.iff (mem x s) (mem x (diff s s')).

      Parameter subset :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> set a -> Prop.

      Axiom subset_def :
        forall {a544 : Type},
        forall {Ih_a544 : Inhab a544},
        forall s : set a544,
        forall s' : set a544,
          Coq.Init.Logic.iff (subset s s') (forall x : a544, mem x s -> mem x s').

      Parameter map :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b) -> set a -> set b.

      Axiom set_map :
        forall {a553 : Type},
        forall {a554 : Type},
        forall {Ih_a553 : Inhab a553},
        forall {Ih_a554 : Inhab a554},
        forall f : a554 -> a553,
        forall s : set a554,
        forall x : a553,
          Coq.Init.Logic.iff (mem x (map f s)) (
              Coq.Init.Logic.ex (
                  fun y : a554 =>
                    Coq.Init.Logic.and (eq (f y) x) (mem y s)
                )
            ).

      Parameter partition :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          (a -> Prop) -> set a -> (set a) * (set a).

      Axiom partition_l_mem :
        forall {a566 : Type},
        forall {Ih_a566 : Inhab a566},
        forall f : a566 -> bool,
        forall s : set a566,
        forall x : a566,
        forall p1 : set a566,
        forall p2 : set a566,
          mem x s -> f x -> eq (partition f s) (p1, p2) -> mem x p1.

      Axiom partition_r_mem :
        forall {a578 : Type},
        forall {Ih_a578 : Inhab a578},
        forall f : a578 -> bool,
        forall s : set a578,
        forall x : a578,
        forall p1 : set a578,
        forall p2 : set a578,
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
        forall {a584 : Type},
        forall {Ih_a584 : Inhab a584},
        forall s : set a584,
          Coq.Init.Logic.iff (finite s) (
              Coq.Init.Logic.ex (
                  fun seq : sequence a584 =>
                    forall x : a584,
                      mem x s -> Sequence.mem x seq
                )
            ).

      Axiom cardinal_nonneg :
        forall {a586 : Type},
        forall {Ih_a586 : Inhab a586},
        forall s : set a586,
          ge (cardinal s) (0)%Z.

      Axiom cardinal_empty :
        forall {a588 : Type},
        forall {Ih_a588 : Inhab a588},
          eq (cardinal (@empty a588 Ih_a588)) (0)%Z.

      Axiom cardinal_remove :
        forall {a600 : Type},
        forall {Ih_a600 : Inhab a600},
        forall s : set a600,
        forall x : a600,
          finite s ->
          (
            if classicT (mem x s) then
              eq (cardinal (remove x s)) (minus (cardinal s) (1)%Z)
            else
              eq (cardinal (remove x s)) (cardinal s)
          ).

      Axiom cardinal_add :
        forall {a612 : Type},
        forall {Ih_a612 : Inhab a612},
        forall s : set a612,
        forall x : a612,
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

      Axiom of_seq_mem :
        forall {a618 : Type},
        forall {Ih_a618 : Inhab a618},
        forall s : sequence a618,
        forall x : a618,
          Coq.Init.Logic.iff (mem x (of_seq s)) (Sequence.mem x s).

      Parameter to_seq :
        forall {a : Type},
        forall {Ih_a : Inhab a},
          set a -> sequence a.

      Axiom to_seq_mem :
        forall {a657 : Type},
        forall {Ih_a657 : Inhab a657},
        forall s : set a657,
          finite s ->
          (
            forall x : a657,
              Coq.Init.Logic.iff (mem x s) (eq (Sequence.multiplicity x (to_seq s)) (1)%Z)
          ).

      Parameter fold :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b) -> (b -> b -> b) -> set a -> b -> b.

      Axiom fold_def :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
        forall f : a -> b,
        forall m : b -> b -> b,
        forall s : set a,
        forall acc : b,
          finite s ->
          comm_monoid m acc ->
          eq (fold f m s acc) (
              Sequence.fold_right (fun x : a => fun acc : b => m (f x) acc) (to_seq s) acc
            ).

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
        forall {a646 : Type},
        forall {a647 : Type},
        forall {Ih_a646 : Inhab a646},
        forall {Ih_a647 : Inhab a647},
        forall x : a647,
        forall m : a647 -> a646,
        forall default : a646,
          Coq.Init.Logic.not (eq (m x) default) -> _Set.mem x (domain default m).

    End Map.


End Stdlib.
