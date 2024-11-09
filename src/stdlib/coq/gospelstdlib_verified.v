Set Implicit Arguments.

From TLC Require Import LibString LibList LibCore LibListZ LibEpsilon LibSet.

Require Import Coq.ZArith.BinInt TLC.LibLogic TLC.LibRelation TLC.LibInt TLC.LibContainer.

Require Import Coq.ZArith.BinIntDef.

Delimit Scope Z_scope with Z.

Require gospelstdlib_mli.
Module Stdlib.

  Module Gospelstdlib : gospelstdlib_mli.Stdlib.

    Definition sequence A := list A.

    Definition bag A := A -> nat.
    
    Definition set A := A -> Prop.

    Definition map A B := A -> B.

    Definition succ := Z.succ.
    Definition pred := Z.pred.
    Definition neg := Z.opp.
    Definition plus := Z.add.
    Definition minus := Z.sub.
    Definition mult := Z.mul.
    Definition div  := Z.div.
    Definition _mod := Z.modulo.
    Definition pow := Z.pow.
    Definition abs := Z.abs.
    Definition min := Z.min.
    Definition max := Z.max.
    Definition gt := gt.
    Definition ge := ge.
    Definition lt := lt.
    Definition le := le.
    Parameter max_int : Z.
    Parameter min_int : Z.
    Definition app A {Ih:Inhab A} := @app A.
    
    Definition seq_get
      {A} {Ih:Inhab A} (s : sequence A) (n : int) : A :=
      s[n].

    Ltac unfold_all :=
      unfold pred in *;
      unfold neg in * ;
      unfold plus in *;
      unfold minus in *;
      unfold mult in *;
      unfold div  in *;
      unfold _mod in *;
      unfold pow in * ;
      unfold abs in * ;
      unfold min in * ;
      unfold max in * ;
      unfold gt in *  ;
      unfold ge in *  ;
      unfold lt in *  ;
      unfold le in *  ;
      unfold app in * ;
      unfold seq_get in *.

    
    Parameter of_list :
      forall a : Type,
      forall {aIh : Inhab a},
        list a -> sequence a.
    
    Definition seq_sub {A} {Ih : Inhab A} (s : sequence A) (i1 : Z) (i2 : Z) : sequence A :=
      LibListZ.take (i2 - i1) (LibListZ.drop i1 s).
    
    
    Definition seq_sub_l  (A : Type) {Ih : Inhab A} (s : sequence A) (
        i : Coq.Numbers.BinNums.Z
      ) : sequence A:=
      seq_sub s i (length s).

    Definition seq_sub_r  (A : Type) {Ih : Inhab A} (s : sequence A) (
        i : Coq.Numbers.BinNums.Z
      ) : sequence A:=
      seq_sub s (0)%Z i.

    Definition map_set  (A : Type) (B : Type) {Ih : Inhab A} {Ih : Inhab B} (f : A -> B) (x : A) (y : B) : A -> B:=
      fun arg : A =>
        if classicT (Coq.Init.Logic.eq arg x) then y else f x.


    Definition monoid {A} `{Inhab A} (f : A -> A -> A) (n : A) : Prop :=
      neutral_l f n /\ neutral_r f n /\ assoc f.

    Lemma monoid_def :
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
    Proof.
      intros A Ih f neutral.
      unfold monoid.
      unfold neutral_l, neutral_r, assoc.
      split.
      - intros [H1 [H2 H3]]. split.
        + intros x. rewrite H1. rewrite H2. auto.
        + auto.
      - intros [H1 H2]. 
        repeat split; intros x; specialize H1 with x as [H3 H4];
        try (rewrite H3); auto.
    Qed.

    Definition comm_monoid {A} `{Inhab A} (f : A -> A -> A) (n : A) : Prop :=
      monoid f n /\ comm f.

    Lemma comm_monoid_def :
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
    Proof.
      intros A Ih f neutral.
      unfold comm_monoid, monoid, comm.
      tauto.
    Qed.

    Module Sequence.

      Parameter t : Type -> Type.

      Definition in_range  (A : Type) {Ih : Inhab A} (s : sequence A) (
          i : Coq.Numbers.BinNums.Z
        ) : Prop:=
        Coq.Init.Logic.and (le (0)%Z i) (lt i (length s)).

      Lemma in_range_def :
        forall {a4 : Type},
        forall {Ih_a4 : Inhab a4},
        forall s : sequence a4,
        forall i : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.iff (in_range s i) (
              Coq.Init.Logic.and (le (0)%Z i) (lt i (length s))
            ).
      Proof.
        unfold in_range.
        tauto.
      Qed.
            
      Definition length {A} {Ih : Inhab A} (s : sequence A) : Z :=
        LibListZ.length s.

      Lemma length_nonneg :
        forall A11 : Type,
        forall {Ih : Inhab A11},
        forall s : sequence A11,
          le (0)%Z (length s).
      Proof.
        intros.
        unfold le.
        unfold length.
        math.
      Qed.
      
      Lemma append_length :
        forall A17 : Type,
        forall {Ih : Inhab A17},
        forall s : sequence A17,
        forall s' : sequence A17,
          Coq.Init.Logic.eq (length (app s s')) (plus (length s) (length s')).
      Proof.
        intros.
        unfold_all.
        unfold length.
        unfold app.
        rew_list.
        math.
      Qed.
      
      Lemma append_elems_left :
        forall A26 : Type,
        forall {Ih : Inhab A26},
        forall s : sequence A26,
        forall s' : sequence A26,
        forall i : Coq.Numbers.BinNums.Z,
          in_range s i ->
          Coq.Init.Logic.eq (seq_get (app s s') i) (seq_get s i).
      Proof.
        unfold in_range.
        intros.
        unfold_all.
        unfold length in *.
        rewrite read_app.
        rewrite If_l.
        + auto.
        + math.
      Qed.
      
      Lemma append_elems_right :
        forall a36 : Type,
        forall {a36Ih : Inhab a36},
        forall s : sequence a36,
        forall s' : sequence a36,
        forall i : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.and (le (length s) i) (
              lt i (plus (length s) (length s'))
            ) ->
          Coq.Init.Logic.eq (seq_get (app s s') i) (
              seq_get s' (minus i (length s))
            ).
      Proof.
        unfold length.
        intros.
        unfold_all.
        rewrite read_app.
        rewrite If_r.
        + auto.
        + math.
      Qed.

      Lemma subseq_l :
        forall {a40 : Type},
        forall {_Ga40 : Inhab a40},
        forall s : sequence a40,
        forall i : Coq.Numbers.BinNums.Z,
          in_range s i ->
          Coq.Init.Logic.eq (seq_sub_l s i) (seq_sub s i (length s)).
      Proof.
        auto.
      Qed.
        
      Lemma subseq_r :
        forall {a44 : Type},
        forall {Ih_a44 : Inhab a44},
        forall s : sequence a44,
        forall i : Coq.Numbers.BinNums.Z,
          in_range s i -> Coq.Init.Logic.eq (seq_sub_r s i) (seq_sub s (0)%Z i).
      Proof.
        auto.
      Qed.
      
      Lemma subseq :
        forall {a50 : Type},
        forall {_Ga50 : Inhab a50},
        forall s : sequence a50,
        forall i : Coq.Numbers.BinNums.Z,
        forall i1 : Coq.Numbers.BinNums.Z,
        forall i2 : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.and (le (0)%Z i1) (
              Coq.Init.Logic.and (le i1 i) (
                  Coq.Init.Logic.and (lt i i2) (le i2 (length s))
                )
            ) ->
          Coq.Init.Logic.eq (seq_get s i) (seq_get (seq_sub s i1 i2) (minus i i1)).
      Proof.
        unfold_all.
        unfold length.
        unfold seq_sub.
        intros A IhA s i i1 i2 [H1 [H2 [H3 H4]]].
        rewrite read_take; try (split; math).
        - rewrite read_drop; try (split; math).
          f_equal.
          math.
        - rewrite LibListZ.length_drop; math.        
      Qed.
        
      Lemma subseq_len :
        forall {a60 : Type},
        forall {Ih_a60 : Inhab a60},
        forall s : sequence a60,
        forall i1 : Coq.Numbers.BinNums.Z,
        forall i2 : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.and (le (0)%Z i1) (
              Coq.Init.Logic.and (le i1 i2) (lt i2 (length s))
            ) ->
          Coq.Init.Logic.eq (length (seq_sub s i1 i2)) (minus i2 i1).
      Proof.
        unfold length.
        unfold_all.
        intros A IhA s i1 i2 [H1 [H2 H3]].
        unfold seq_sub.
        rewrite LibListZ.length_take; try math.
        rewrite LibListZ.length_drop; try math.
      Qed.
        
      Fixpoint init_aux {A} (n : nat) (f : Z -> A) : sequence A :=
        match n with
        |O => nil
        |S n' =>  LibList.app (init_aux n' f) ((cons (f n') nil)) end.

      Definition init {A} {Ih : Inhab A} (n : Z) (f : Z -> A) : sequence A :=
        If n < 0 then arbitrary else
        init_aux (Z.to_nat n) f.

      Lemma init_pos :
        forall A {Ih : Inhab A} n (f : Z -> A),
          n >= 0 -> init n f = init_aux (Z.to_nat n) f.
      Proof.
        intros A Ih n f H.
        unfold init. rewrite If_r. auto. math.
      Qed.

      Lemma init_aux_length :
        forall A n (f : Z -> A),
          LibListZ.length (init_aux n f) = n.
      Proof.
        intros A n f.
        induction n as [|n' Ih].
        + simpl. rew_list. math.
        + simpl. rew_list.
          rewrite Ih. math.
      Qed.
      
      Lemma init_length :
        forall A49 : Type,
        forall {Ih : Inhab A49},
        forall n : Coq.Numbers.BinNums.Z,
        forall f : Coq.Numbers.BinNums.Z -> A49,
          ge n 0 -> Coq.Init.Logic.eq (length (init n f)) n.
      Proof.
        intros A Ih n f H.
        unfold init. unfold ge in *.
        rewrite If_r.
        - unfold length. rewrite init_aux_length. math.
        - math.
      Qed.

      Lemma init_i :
        forall A (n : nat) (i : Z) (f : Z -> A),
        forall {Ih : Inhab A},
          0 <= i ->
          i < n ->
          (init_aux n f)[i] = f i.
      Proof.
        intros A n i f Inh H1 H2.
        induction n as [|n' Ih].
        - math.
        - simpl. rewrite read_app.
          rewrite init_aux_length.
          assert (P : i < n' \/ ~ i < n').
          apply classic.
          destruct P as [P|P].
          + rewrite If_l. auto. math.
          +  assert (Q : i = n'). { math. } rewrite If_r.
             * rewrite Q. rewrite Z.sub_diag. apply read_zero.
             * math.
      Qed.

      Lemma init_elems :
        forall A57 : Type,
        forall {Ih : Inhab A57},
        forall n : Coq.Numbers.BinNums.Z,
        forall f : Coq.Numbers.BinNums.Z -> A57,
        forall i : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.and (le (0)%Z i) (lt i n) ->
          Coq.Init.Logic.eq (seq_get (init n f) i) (f i).
      Proof.
        intros A Inh n f i [H1 H2].
        unfold seq_get.
        unfold init.
        unfold le in *.
        unfold lt in *.
        rewrite If_r; try math.
        apply init_i; math.
      Qed.

      Definition empty {A} {Ih : Inhab A} := @nil A.
      Lemma empty_length :
        forall A {Ih : Inhab A}, Coq.Init.Logic.eq (length (@empty A Ih)) (0)%Z.
      Proof.
        intro A.
        unfold length. rew_list. auto.
      Qed.

      Definition singleton (A : Type) {Ih : Inhab A} (x : A) : sequence A:=
        init (1)%Z (fun _ : Coq.Numbers.BinNums.Z => x).

      
      Lemma singleton_def :
        forall {a82 : Type},
        forall {Ih_a82 : Inhab a82},
        forall x : a82,
        forall f : Coq.Numbers.BinNums.Z -> a82,
          Coq.Init.Logic.eq (f (0)%Z) x ->
          Coq.Init.Logic.eq (singleton x) (init (1)%Z f).
      Proof.
        intros A IhA x f H1.
        unfold singleton, init.
        repeat (rewrite If_r; try math;
        simpl; rew_list).
        f_equal.
        rewrite <- H1.
        auto.
      Qed.
      
      Definition cons (A : Type) {Ih : Inhab A} (x : A) (s : sequence A) : sequence A:=
        x :: s.

      Lemma cons_def :
        forall {a88 : Type},
        forall {Ih_a88 : Inhab a88},
        forall x : a88,
        forall s : sequence a88,
          Coq.Init.Logic.eq (cons x s) (app (singleton x) s).
      Proof.
        intros A Ih x s.
        unfold app, cons, singleton, init.
        rewrite If_r; try math.
        simpl. rew_list. auto.
      Qed.
      
      Definition snoc (A : Type) {Ih : Inhab A}(s : sequence A) (x : A) : sequence A:=
        app s (singleton x).

      Lemma snoc_def :
        forall {a94 : Type},
        forall {Ih_a94 : Inhab a94},
        forall s : sequence a94,
        forall x : a94,
          Coq.Init.Logic.eq (snoc s x) (app s (singleton x)).
      Proof.
        auto.
      Qed.
      
      Definition hd (A : Type) {Ih : Inhab A} (s : sequence A) : A:= seq_get s (0)%Z.

      Lemma hd_def :
        forall {a99 : Type},
        forall {Ih_a99 : Inhab a99},
        forall s : sequence a99,
          Coq.Init.Logic.eq (hd s) (seq_get s (0)%Z).
      Proof.
        auto.
      Qed.
      
      Definition tl (A : Type){Ih : Inhab A} (s : sequence A) : sequence A:=
        seq_sub_l s (1)%Z.


      Lemma tl_def :
        forall {a102 : Type},
        forall {Ih_a102 : Inhab a102},
        forall s : sequence a102,
          Coq.Init.Logic.eq (tl s) (seq_sub_l s (1)%Z).
      Proof.
        auto.
      Qed.

      Definition append  (A : Type){Ih : Inhab A} (s1 : sequence A) (s2 : sequence A) : sequence A:=
        app s1 s2.


      Lemma append_def :
        forall {a107 : Type},
        forall {Ih_a107 : Inhab a107},
        forall s1 : sequence a107,
        forall s2 : sequence a107,
          Coq.Init.Logic.eq (append s1 s2) (app s1 s2).
      Proof.
        auto.
      Qed.
      
      Definition multiplicity {A}{Ih : Inhab A} (e : A) (s : sequence A) : Z :=
        LibListZ.count (fun x => x = e) s.

      Lemma mult_empty :
        forall A73 : Type,
        forall {Ih : Inhab A73},
        forall x : A73,
          Coq.Init.Logic.eq (multiplicity x (@empty A73 Ih)) (0)%Z.
      Proof.
        intros A Ih x.
        unfold multiplicity.
        rew_listx.
        auto.
      Qed.

      Lemma mult_cons :
        forall A79 : Type,
        forall {Ih : Inhab A79},
        forall s : sequence A79,
        forall x : A79,
          Coq.Init.Logic.eq (plus (1)%Z (multiplicity x s)) (
              multiplicity x (cons x s)
            ).
      Proof.
        intros A Ih s x.
        unfold multiplicity.
        unfold plus.
        unfold cons.
        rew_listx.
        rewrite If_l; auto.
        math.
      Qed.

      Lemma mult_cons_neutral :
        forall A {Ih : Inhab A} s (x1 : A) x2,
          Coq.Init.Logic.not (Coq.Init.Logic.eq x1 x2) ->
          Coq.Init.Logic.eq (multiplicity x1 s) (multiplicity x1 (cons x2 s)).
      Proof.
        intros A Ih s x1 x2 H1.
        unfold multiplicity.
        unfold cons.
        rew_listx.
        rewrite If_r.
        + math.
        + auto.
      Qed.

      Lemma mult_length :
        forall A92 : Type,
        forall {Ih: Inhab A92},
        forall x : A92,
        forall s : sequence A92,
          Coq.Init.Logic.and (le (0)%Z (multiplicity x s)) (
              le (multiplicity x s) (length s)
            ).
      Proof.
        intros A I x s.
        unfold le.
        unfold multiplicity.
        unfold length.
        split.
        - apply count_nonneg.
        - induction s as [|e s' Ih].
          + rew_listx. math.
          + rew_listx.
            assert (E : e = x \/ e <> x). apply classic.
            destruct E;
              [rewrite If_l | rewrite If_r]; auto; math.
      Qed.

      Definition mem  (A : Type) {Ih : Inhab A} (x : A) (s : sequence A) : Prop:= LibList.mem x s.

      Lemma mem_def :
        forall {a134 : Type},
        forall {Ih_a134 : Inhab a134},
        forall s : sequence a134,
        forall x : a134,
          Coq.Init.Logic.iff (mem x s) (gt (multiplicity x s) (0)%Z).
      Proof.
        intros A IhA s x.
        unfold_all.
        unfold multiplicity.
        unfold mem.
        split; intros H1.
        + change (LibListZ.count (= x) s > 0%nat).
          rewrite <- LibListZ.Exists_eq_count_pos.
          rewrite Exists_eq_exists_mem.
          exists x; auto.
        + change (LibListZ.count (= x) s > 0%nat) in H1.
          rewrite <- LibListZ.Exists_eq_count_pos in H1.
          rewrite Exists_eq_exists_mem in H1.
          destruct H1 as [y [H1 H2]].
          subst. auto.
      Qed.


      Definition _forall {A} `{Inhab A} (P : A -> Prop) (s : sequence A) :=
        Forall P s.

      Lemma forall_def :
        forall {a176 : Type},
        forall {Ih_a176 : Inhab a176},
        forall p : a176 -> bool,
        forall s : sequence a176,
          Coq.Init.Logic.iff (_forall p s) (forall x : a176, mem x s -> p x).
      Proof.
        intros A Ih p s.
        unfold mem, _forall.
        rewrite Forall_eq_forall_mem. tauto.
      Qed.

      Definition _exists {A} `{Inhab A} (P : A -> Prop) (s : sequence A) :=
        Exists P s.

      Lemma _exists_def :
        forall {a182 : Type},
        forall {Ih_a182 : Inhab a182},
        forall p : a182 -> bool,
        forall s : sequence a182,
          Coq.Init.Logic.iff (_exists p s) (
              Coq.Init.Logic.ex (fun x : a182 => Coq.Init.Logic.and (mem x s) (p x))
            ).
      Proof.
        intros A Ih p s.
        unfold _exists, mem.
        rewrite Exists_eq_exists_mem.
        tauto.
      Qed.

      Definition map {A} {B} {Ih:Inhab A} {Ih:Inhab B} := @LibList.map A B.

      Lemma map_elems :
        forall A100 : Type,
        forall A102 : Type,
        forall {Ih : Inhab A100},
        forall {Ih : Inhab A102},
        forall i : Coq.Numbers.BinNums.Z,
        forall f : A100 -> A102,
        forall s : sequence A100,
          in_range s i -> eq (seq_get (map f s) i) (f (seq_get s i)).
      Proof.
        intros A B IhA IhB i f s [H1 H2].
        unfold seq_get.
        unfold le in *. unfold lt in *.
        repeat (rewrite If_r; try math).
        unfold map.
        apply LibListZ.read_map. unfold length in *.
        rew_index. split; auto.        
      Qed.
      
      Definition filter {A} {Ih : Inhab A} := @LibList.filter A.

      Lemma filter_elems :
        forall A113 : Type,
        forall {Ih : Inhab A113},
        forall f : A113 -> bool,
        forall s : sequence A113,
        forall x, mem x s -> f x -> mem x (filter f s).
      Proof.
        intros A IhA f s.
        unfold mem in *. unfold multiplicity in *.
        unfold gt in *.
        unfold filter in *.
        induction s as [|e t IHt]; intros x H1 H2.
        - inversion H1. 
        - rew_listx.
          inversion H1; subst; auto.
      Qed.


      Definition filter_map
        {A} {B} {IhA : Inhab A} {IhB : Inhab B} 
        (f : A -> option B) (s : sequence A) : sequence B :=
        let g := 
          fun x => 
            match f x with 
            |Some x => x |None => arbitrary end in
        LibList.map g (LibList.filter (fun x => f x <> None) s).
        
      Lemma filter_map_elems :
        forall {a707 : Type},
        forall {a708 : Type},
        forall {Ih_a707 : Inhab a707},
        forall {Ih_a708 : Inhab a708},
        forall f : a707 -> option a708,
        forall s : sequence a707,
        forall y : a708,
          (exists x, f x = Some y /\ mem x s) <->
            mem y (filter_map f s).
      Proof.
        intros A B IhA IhB f s y.
        unfold filter_map.
        split; intros H1.
        - destruct H1 as [x [H1 H2]]. 
          apply mem_map' with x.
          + apply mem_filter; auto.
            rewrite H1. discriminate.
          + rewrite H1. auto.
        - apply LibList.mem_Nth 
            with B (LibList.map (*very awkward :|*)
                      (fun x : A =>
                         match f x with
                         | Some x0 => x0
                         | None => arbitrary
                         end)
                      (LibList.filter
                         (fun x : A => f x <> None)
                         s)) y in H1.
          destruct H1 as [n H1].
          apply Nth_map_inv in H1.
          destruct H1 as [x [H1 H2]].
          exists x. apply Nth_mem in H2.
          rewrite mem_filter_eq in H2.
          destruct H2 as [H2 H3].
          split; auto.
          destruct (f x). 
          + rewrite H1. auto.
          + contradiction.
      Qed.

      Definition get {A} {Ih : Inhab A} := @seq_get A Ih.

      Lemma get_def :
        forall {a156 : Type},
        forall {Ih_a156 : Inhab a156},
        forall s : sequence a156,
        forall i : Coq.Numbers.BinNums.Z,
          Coq.Init.Logic.eq (get s i) (seq_get s i).
      Proof.
        auto.
      Qed.
      
      Fixpoint set_aux {A} (s : sequence A) (n : nat) (x : A) : sequence A :=
        match s, n with
        |nil, _ => arbitrary
        |_ :: t, O => x :: t
        |e :: t, S n' => e :: set_aux t n' x
        end.

      Definition set {A} {Ih : Inhab A} (s : sequence A) (n : Z) (x : A) : sequence A :=
        If n < 0 then arbitrary else set_aux s (Z.to_nat n) x.

      Lemma set_aux_len :
        forall A `{Inhab A} (s : sequence A) (i : nat) x,
          0 <= i < Z.to_nat (LibListZ.length s) ->
          LibListZ.length (set_aux s i x) =
            LibListZ.length s.
      Proof.
        intros A IhA s.
        induction s as [|h s Ih];
          intros i x H1;
          rew_listx in H1.
        + math.
        + simpl. rew_listx.
          destruct i; rew_listx; auto.
          rewrite Ih; math.
      Qed.
        
      Lemma set_aux_elem :
        forall A {IhA : Inhab A} s (i : Z) (x : A),
          0 <= i < Z.to_nat (LibListZ.length s) ->
          (set_aux s (to_nat i) x)[i] = x.
      Proof.
        induction s as [|e t Ih]; intros i x [H1 H2].
        - rew_list in H2. math.
        - simpl. remember (to_nat i) as n eqn:E.
          destruct n as [|n'].
          + assert (A1 : i = 0). { math. }
            rewrite A1. rew_listx. auto.
          + assert (A1 : n' = to_nat (i - 1)).
            { math. }
            rewrite read_cons_pos; try math.
            rewrite A1.
            rew_list in H2.          
            rewrite Ih.
            * auto.
            * math.
      Qed.
      
      Lemma set_elem :
        forall A121 : Type,
        forall {Ih : Inhab A121},
        forall s : sequence A121,
        forall i : Coq.Numbers.BinNums.Z,
        forall x : A121,
          in_range s i ->
          Coq.Init.Logic.eq (seq_get (set s i x) i) x.
      Proof.
        intros A IHA s i x [H1 H2].
        unfold le in *. unfold lt in *. unfold seq_get in *. unfold length in *.
        unfold set. rewrite If_r; try math.
        apply set_aux_elem.
        split; math.
      Qed.

      Lemma set_aux_elem_other :
        forall A {Ih :Inhab A} s i1 i2 (x : A),
          i1 <> i2 ->
          0 <= i1 < LibListZ.length s ->
          0 <= i2 < LibListZ.length s ->
          (set_aux s (Z.to_nat i1) x)[i2] = s[i2].
      Proof.
        intros A IhA s.
        induction s as [|e s Ih]; intros i1 i2 x H1 H2 H3;
          rew_list in *.
        - math.
        - simpl.
          remember (to_nat i1) as n1 eqn:E1.
          remember (to_nat i2) as n2 eqn:E2.
          destruct n1 as [|n1'].
          + destruct n2 as [|n2'].
            * math.
            * rewrite read_cons_pos; try math.
              rewrite read_cons_pos; try math.
              auto.
          + destruct n2 as [|n2'].
            * assert (A1 : i2 = 0). { math. }
              rewrite A1. rew_list. reflexivity.
            * rewrite read_cons_pos; try math.
              rewrite read_cons_pos; try math.
              assert (A1 : n1' = to_nat (i1 - 1)).
              { math. }
              rewrite A1.
              rewrite Ih; auto; math.
      Qed.

      Lemma set_elem_other :
        forall A130 : Type,
        forall {Ih : Inhab A130},
        forall s : sequence A130,
        forall i1 : Coq.Numbers.BinNums.Z,
        forall i2 : Coq.Numbers.BinNums.Z,
        forall x : A130,
          Coq.Init.Logic.not (Coq.Init.Logic.eq i1 i2) ->
          in_range s i1 ->
          in_range s i2 ->
          Coq.Init.Logic.eq (seq_get (set s i1 x) i2) (seq_get s i2).
      Proof.
        unfold in_range.
        intros A IhA s i1 i2 x H1 H2 H3.
        unfold le in *. unfold lt in *. unfold length in *.
        unfold seq_get. unfold set. repeat (rewrite If_r; try math).
        apply set_aux_elem_other; math.
      Qed.

      Definition rev {A} {Ih : Inhab A} := @LibList.rev A .

      Lemma rev_length :
        forall A134 : Type,
        forall {Ih: Inhab A134},
        forall s : sequence A134,
          Coq.Init.Logic.eq (length s) (length (rev s)).
      Proof.
        intros A Ih s. unfold length. unfold rev. rew_list. auto.
      Qed.

      Lemma bah :
        forall A `{Inhab A} (s : sequence A) i,
          0 <= i < LibListZ.length s ->
          s[i] = LibList.nth (abs i) s.
      Proof.
        intros A IhA s i H1.
        remember (abs i) as n eqn:E.
        generalize dependent i.
        generalize dependent s.
        induction n as [|n' Ih]; intros s i H1 H2.
        + assert (A1 : i = 0). { math. } rewrite A1.
          destruct s.
          * rew_list in H1. math.
          * rew_list. rew_listx. auto.
        + destruct s; rew_list in H1.
          *  math.
          * rew_listx.
            rewrite read_cons_pos; try math.
            rewrite Ih; auto; math.
      Qed.

      Lemma rev_elems :
        forall A143 : Type,
        forall {Ih : Inhab A143},
        forall i : Coq.Numbers.BinNums.Z,
        forall s : sequence A143,
          in_range s i ->
          Coq.Init.Logic.eq (seq_get (rev s) i) (
              seq_get s (minus (minus (length s) 1) (i)%Z)
            ).
      Proof.
        unfold in_range.
        intros A IhA i s [H1 H2].
        unfold_all.
        unfold seq_get.
        unfold length in *.
        unfold rev.
        rewrite bah.
        rewrite bah.
        + remember (abs i) as n eqn:E1.
          assert
            (A1 : abs (LibListZ.length s - 1 - i) = (LibList.length s - 1 - n)%nat).
          { math. }
          rewrite A1.
          apply nth_rev. math.
        + math.
        + rew_list. math.
      Qed.

      Definition fold_left A B {IhA : Inhab A} {IhB : Inhab B} (f : A -> B -> A) (x : A) (s : sequence B) : A  :=
        LibList.fold_left (fun x y => f y x) x s.


      Lemma fold_left_empty :
        forall {a203 : Type},
        forall {a204 : Type},
        forall {Ih_a203 : Inhab a203},
        forall {Ih_a204 : Inhab a204},
        forall f : a204 -> a203 -> a204,
        forall acc : a204,
          Coq.Init.Logic.eq (fold_left f acc (@empty a203 Ih_a203)) acc.
      Proof.
        intros A1 A2 Ih1 Ih2 f acc.
        unfold fold_left.
        rew_list.
        auto.
      Qed.

      Lemma fold_left_cons :
        forall {a215 : Type},
        forall {a216 : Type},
        forall {Ih_a215 : Inhab a215},
        forall {Ih_a216 : Inhab a216},
        forall f : a216 -> a215 -> a216,
        forall acc : a216,
        forall x : a215,
        forall l : sequence a215,
          Coq.Init.Logic.eq (fold_left f acc (cons x l)) (fold_left f (f acc x) l).
      Proof.
        intros A B IhA IhB f acc x l.
        unfold fold_left, cons.
        rew_listx.
        auto.
      Qed.


      Definition fold_right
        {A} {B} `{Inhab A} `{Inhab B}
        (f : A -> B -> B) s acc :=
        LibList.fold_right f acc s.

      Lemma fold_right_empty :
        forall {a221 : Type},
        forall {a222 : Type},
        forall {Ih_a221 : Inhab a221},
        forall {Ih_a222 : Inhab a222},
        forall acc : a222,
        forall f : a221 -> a222 -> a222,
          Coq.Init.Logic.eq (fold_right f (@empty a221 Ih_a221) acc) acc.
      Proof.
        intros A B IhA IhB acc f.
        unfold fold_right.
        unfold empty.
        rew_list.
        auto.
      Qed.

      Lemma fold_right_cons :
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
      Proof.
        intros A B IhA IhB acc f x l.
        unfold fold_right, cons.
        rew_listx.
        auto.
      Qed.
      
      Lemma extensionality :
        forall A171 : Type,
        forall {Ih : Inhab A171},
        forall s1 : sequence A171,
        forall s2 : sequence A171,
          Coq.Init.Logic.eq (length s1) (length s2) ->
          (
            forall i : Coq.Numbers.BinNums.Z,
              in_range s1 i ->
              Coq.Init.Logic.eq (seq_get s1 i) (seq_get s2 i)
          ) -> s1 = s2.
      Proof.
        unfold in_range.
        intros A IhA s1 s2 H1 H2.
        unfold le in *. unfold lt in *. unfold length in *.
        unfold seq_get in *.        
        apply eq_of_extens_range with IhA.
        auto.
        intros n. specialize H2 with n.
        intros H3.
        apply H2. math.
      Qed.

      Definition permut {A} `{Inhab A} (s1 : sequence A) (s2 : sequence A) :=
        (forall x : A, Coq.Init.Logic.iff (mem x s1) (mem x s2)).

      Lemma permut_mem :
        forall {a300 : Type},
        forall {Ih_a300 : Inhab a300},
        forall s1 : sequence a300,
        forall s2 : sequence a300,
          permut s1 s2 ->
          (forall x : a300, Coq.Init.Logic.iff (mem x s1) (mem x s2)).
      Proof.
        tauto.
      Qed.

      Definition permut_sub {A} `{Inhab A} 
        (s1 : sequence A) (s2 : sequence A) (i : Z) (j : Z) :=
        permut (seq_sub s1 i j) (seq_sub s2 i j) /\
          eq (seq_sub_r s1 i) (seq_sub_r s2 i) /\
          eq (seq_sub_l s1 j) (seq_sub_l s2 j).

      Lemma permut_sub_def :
        forall {a314 : Type},
        forall {Ih_a314 : Inhab a314},
        forall s1 : sequence a314,
        forall s2 : sequence a314,
        forall i : Z,
        forall j : Z,
          permut (seq_sub s1 i j) (seq_sub s2 i j) ->
          eq (seq_sub_r s1 i) (seq_sub_r s2 i) ->
          eq (seq_sub_l s1 j) (seq_sub_l s2 j) -> permut_sub s1 s2 i j.
      Proof.
        unfold permut_sub. tauto.
      Qed.

    End Sequence.

    Module Bag.

      Parameter t : Type -> Type.

      Definition multiplicity {A} `{Inhab A} (x : A) (b : bag A) : Z :=
        b x.

      Lemma well_formed :
        forall a173 : Type,
        forall {a173Ih : Inhab a173},
        forall b : bag a173,
        forall x : a173,
          ge (multiplicity x b) (0)%Z.
      Proof.
        intros A Ih b x.
        unfold_all.
        unfold multiplicity.
        math.
      Qed.
        
      Definition empty {A} {aIh : Inhab A} := fun (_ : A) => 0%nat.

      Lemma empty_mult :
        forall {a240 : Type},
        forall {Ih_a240 : Inhab a240},
        forall x : a240,
          Coq.Init.Logic.eq (multiplicity x (@empty a240 Ih_a240)) (0)%Z.
      Proof.
        auto.
      Qed.
      
      Definition init {A} `{Inhab A} (f : A -> Z) : bag A :=
        fun x => Z.to_nat(f x).
      
      Lemma init_axiom :
        forall a182 : Type,
        forall {a182Ih : Inhab a182},
        forall f : a182 -> Coq.Numbers.BinNums.Z,
        forall x : a182,
          Coq.Init.Logic.eq (max (0)%Z (f x)) (multiplicity x (init f)).
      Proof.
        unfold_all.
        unfold multiplicity, init.
        math.
      Qed.
      
      Definition add {A} `{Inhab A} (x : A) b : bag A :=
        fun y => If y = x then (1 + b y)%nat else b y.

      Lemma add_mult_x :
        forall a188 : Type,
        forall {a188Ih : Inhab a188},
        forall b : bag a188,
        forall x : a188,
          Coq.Init.Logic.eq (multiplicity x (add x b)) (
              plus (1)%Z (multiplicity x b)
            ).
      Proof.
        intros A IhA b x.
        unfold_all.
        unfold multiplicity, add.
        rewrite If_l; auto.
        math.
      Qed.
        
      Lemma add_mult_neg_x :
        forall a196 : Type,
        forall {a196Ih : Inhab a196},
        forall x : a196,
        forall y : a196,
        forall b : bag a196,
          Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
          Coq.Init.Logic.eq (multiplicity y (add x b)) (multiplicity y b).
        intros A IhA x y b H1.
        unfold multiplicity, add.
        rewrite If_r; auto.
      Qed.

      Definition singleton  (a : Type) { aIh : Inhab a } (x : a) : bag a:=
        add x (@empty a aIh).

      Lemma singleton_def :
        forall {a269 : Type},
        forall {Ih_a269 : Inhab a269},
        forall x : a269,
          Coq.Init.Logic.eq (singleton x) (add x (@empty a269 Ih_a269)).
      Proof.
        auto.
      Qed.
      
      Definition mem  (a : Type) { aIh : Inhab a } (x : a) (b : bag a) : Prop:=
        gt (multiplicity x b) (0)%Z.

      Lemma mem_def :
        forall {a251 : Type},
        forall {Ih_a251 : Inhab a251},
        forall x : a251,
        forall b : bag a251,
          Coq.Init.Logic.iff (mem x b) (gt (multiplicity x b) (0)%Z).
      Proof.
        tauto.
      Qed.
      
      Definition remove {A} `{Inhab A} (x : A) b : bag A :=
        fun y => If y = x then Nat.pred (b y) else b y.

      Lemma remove_mult_x :
        forall a205 : Type,
        forall {a205Ih : Inhab a205},
        forall b : bag a205,
        forall x : a205,
          Coq.Init.Logic.eq (multiplicity x (remove x b)) (
              max (0)%Z (minus (multiplicity x b) (1)%Z)
            ).
      Proof.
        intros A IhA b x.
        unfold_all.
        unfold multiplicity, remove.
        rewrite If_l; auto.
        math.
      Qed.
      
      Lemma remove_mult_neg_x :
        forall a213 : Type,
        forall {a213Ih : Inhab a213},
        forall x : a213,
        forall y : a213,
        forall b : bag a213,
          Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
          Coq.Init.Logic.eq (multiplicity y (remove x b)) (multiplicity y b).
      Proof.
        intros A IhA x y b H1.
        unfold multiplicity, remove.
        rewrite If_r; auto.
      Qed.
        
      Definition union {A} `{Inhab A} (b1 : bag A) b2 :=
        fun x => Nat.max (b1 x) (b2 x).

      Lemma union_all :
        forall a221 : Type,
        forall {a221Ih : Inhab a221},
        forall b : bag a221,
        forall b' : bag a221,
        forall x : a221,
          Coq.Init.Logic.eq (max (multiplicity x b) (multiplicity x b')) (
              multiplicity x (union b b')
            ).
      Proof.
        unfold_all.
        unfold union, multiplicity.
        math.
      Qed.
        
      Definition sum {A} `{Inhab A} (b1 : bag A) b2 :=
        fun x => (b1 x + b2 x)%nat.        
        
      Lemma sum_all :
        forall a229 : Type,
        forall {a229Ih : Inhab a229},
        forall b : bag a229,
        forall b' : bag a229,
        forall x : a229,
          Coq.Init.Logic.eq (plus (multiplicity x b) (multiplicity x b')) (
              multiplicity x (sum b b')
            ).
      Proof.
        unfold plus, multiplicity, sum.
        math.
      Qed.

      Definition inter {A} `{Inhab A} (b1 : bag A) b2 :=
        fun x => Nat.min (b1 x) (b2 x).

      Lemma inter_all :
        forall a237 : Type,
        forall {a237Ih : Inhab a237},
        forall b : bag a237,
        forall b' : bag a237,
        forall x : a237,
          Coq.Init.Logic.eq (min (multiplicity x b) (multiplicity x b')) (
              multiplicity x (inter b b')
            ).
      Proof.
        unfold min, multiplicity, inter.
        math.
      Qed.
      
      Definition diff {A} `{Inhab A} (b1 : bag A) b2 :=
        fun x => (b1 x - b2 x)%nat.

      Lemma diff_all :
        forall a245 : Type,
        forall {a245Ih : Inhab a245},
        forall b : bag a245,
        forall b' : bag a245,
        forall x : a245,
          Coq.Init.Logic.eq (
              max (0)%Z (minus (multiplicity x b) (multiplicity x b'))
            ) (multiplicity x (diff b b')).
      Proof.
        unfold multiplicity, max, minus, diff.
        math.
      Qed.
        
      Definition disjoint  (a : Type) { aIh : Inhab a } (b : bag a) (
          b' : bag a
        ) : Prop:=
        forall x : a,
          mem x b -> Coq.Init.Logic.not (mem x b').


      Lemma disjoint_def :
        forall {a314 : Type},
        forall {Ih_a314 : Inhab a314},
        forall b : bag a314,
        forall b' : bag a314,
          Coq.Init.Logic.iff (disjoint b b') (
              forall x : a314,
                mem x b -> Coq.Init.Logic.not (mem x b')
            ).
      Proof.
        tauto.
      Qed.
      
      Definition subset  (a : Type) { aIh : Inhab a } (b : bag a) (b' : bag a) : Prop:=
        forall x : a,
          le (multiplicity x b) (multiplicity x b').


      Lemma subset_def :
        forall {a328 : Type},
        forall {Ih_a328 : Inhab a328},
        forall b : bag a328,
        forall b' : bag a328,
          Coq.Init.Logic.iff (subset b b') (
              forall x : a328,
                le (multiplicity x b) (multiplicity x b')
            ).
      Proof.
        tauto.
      Qed.
      
      Definition filter {A} `{Inhab A} p (b : bag A) :=
        fun x => If p x then b x else 0%nat.

      Lemma filter_mem :
        forall a259 : Type,
        forall {a259Ih : Inhab a259},
        forall b : bag a259,
        forall x : a259,
        forall f : a259 -> bool,
          f x ->
          Coq.Init.Logic.eq (multiplicity x (filter f b)) (multiplicity x b).
      Proof.
        intros A IhA b x f H1.
        unfold multiplicity, filter.
        rewrite If_l; auto.
      Qed.
             
      Lemma filter_mem_neg :
        forall a266 : Type,
        forall {a266Ih : Inhab a266},
        forall b : bag a266,
        forall x : a266,
        forall f : a266 -> bool,
          Coq.Init.Logic.not (f x) ->
          Coq.Init.Logic.eq (multiplicity x (filter f b)) (0)%Z.
      Proof.
        intros A Ih b x f H1.
        unfold multiplicity, filter.
        rewrite If_r; auto.
      Qed.
      
      Definition cover {A} (b : bag A) l :=
        forall x, b x = LibList.count (fun y => y = x) l.

      Import LibEpsilon.
      
      Definition cardinal {A} `{Inhab A} (b : bag A) : Z :=
         epsilon (fun n : nat => exists l, cover b l /\ n = LibList.length l).
      
      Definition finite  (A : Type) { aIh : Inhab A } (b : bag A) : Prop:=
          exists l, forall x,
            mem x b -> Sequence.mem x l.


      Definition finite_def :
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
        tauto.
      Qed.
        
      
      Parameter card_nonneg :
        forall a273 : Type,
        forall {a273Ih : Inhab a273},
        forall b : bag a273,
          ge (cardinal b) (0)%Z.

      Parameter card_empty :
        forall a276 : Type,
        forall {a276Ih : Inhab a276},      
          Coq.Init.Logic.eq (cardinal (@empty a276 a276Ih)) (0)%Z.
        
      Parameter card_singleton :
        forall a280 : Type,
        forall {a280Ih : Inhab a280},
        forall x : a280,
          Coq.Init.Logic.eq (cardinal (singleton x)) (1)%Z.
          
      Parameter card_union :
        forall a289 : Type,
        forall {a289Ih : Inhab a289},
        forall b1 : bag a289,
        forall b2 : bag a289,
          finite b1 ->
          finite b2 ->
          Coq.Init.Logic.eq (cardinal (union b1 b2)) (
              plus (cardinal b1) (cardinal b2)
            ).
      
      Parameter card_add :
        forall a296 : Type,
        forall {a296Ih : Inhab a296},
        forall x : a296,
        forall b : bag a296,
          finite b ->
          Coq.Init.Logic.eq (cardinal (add x b)) (plus (cardinal b) (1)%Z).

      Parameter card_map :
        forall a303 : Type,
        forall {a303Ih : Inhab a303},
        forall f : a303 -> bool,
        forall b : bag a303,
          finite b -> le (cardinal (filter f b)) (cardinal b).

      Definition of_seq {A} `{Inhab A}
        (s : sequence A) : bag A :=
        fun x => LibList.count (= x) s.

      Lemma of_seq_multiplicity :
        forall a308 : Type,
        forall {a308Ih : Inhab a308},
        forall s : sequence a308,
        forall x : a308,
          Coq.Init.Logic.eq (Sequence.multiplicity x s) (
              multiplicity x (of_seq s)
            ).
      Proof.
        unfold multiplicity.
        unfold Sequence.multiplicity.
        unfold of_seq.
        unfold LibListZ.count.
        math.
      Qed.

      
      Parameter fold :
        forall {a : Type},
        forall {b : Type},
        forall {Ih_a : Inhab a},
        forall {Ih_b : Inhab b},
          (a -> b -> b) -> bag a -> b -> b.

    End Bag.

    Definition set_create {A} {Ih : Inhab A} := fun (_:A) => False.

    Module _Set.
      Definition t := set.
      
      Import TLC.LibSet.

      Definition mem {A} {Ih : Inhab A} (x : A) (s : set A) : Prop := is_in x s.
      Definition empty {A} {Ih : Inhab A} : set A := empty.
      Lemma empty_mem :
        forall A {Ih : Inhab A} x,
          ~ mem x (@empty A Ih).
        intros. unfold empty. auto.
      Qed.
      Definition add {A} {Ih : Inhab A} (x : A) (s : set A) : set A := s \u (single x).
      Lemma add_mem :
        forall A {Ih : Inhab A} s (x : A), mem x (add x s).
        intros. unfold mem. unfold add. rewrite set_in_union_eq.
        right. rewrite in_single_eq. auto. 
      Qed.
      
      Lemma add_mem_neq :
        forall a323 : Type,
        forall {a323Ih : Inhab a323},
        forall s : set a323,
        forall x : a323,
        forall y : a323,
          Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
          Coq.Init.Logic.iff (mem x s) (mem x (add y s)).
      Proof.
        intros A Ih s x y Neq.
        split; intro H; unfold add in *; unfold mem in *.
        - rewrite set_in_union_eq. auto.
        - rewrite set_in_union_eq in H. destruct H.
          + auto.
          + rewrite in_single_eq in H. contradiction.
      Qed.

      Definition singleton  (a : Type) { aIh : Inhab a } (x : a) : set a:=
        add x (@empty a aIh).

      Definition remove {A} {Ih : Inhab A} (x : A) (s : set A) : set A :=
        s \-- x.

      Lemma remove_mem :
        forall a329 : Type,
        forall {a329Ih : Inhab a329},
        forall s : set a329,
        forall x : a329,
          Coq.Init.Logic.not (mem x (remove x s)).
      Proof.
        intros A Ih s x.
        unfold remove, mem, singleton, empty, add.
        rewrite set_in_remove_eq.
        unfold not. intros [H1 H2].
        destruct H2. rewrite set_in_single_eq. auto.
      Qed.
      
      Lemma remove_mem_neq :
        forall a336 : Type,
        forall {a336Ih : Inhab a336},
        forall s : set a336,
        forall x : a336,
        forall y : a336,
          Coq.Init.Logic.not (Coq.Init.Logic.eq x y) ->
          Coq.Init.Logic.iff (mem x s) (mem x (remove y s)).
        intros A Ih s x y Neq. split; intro H;
          unfold mem, remove, singleton, add, empty in *;
          rewrite set_in_remove_eq in *.
        split. auto. unfold not. intros H1. contradiction.
        - destruct H. auto.
      Qed.

      Definition union {A} {Ih : Inhab A} (s1 : set A) (s2 : set A) : set A :=
        s1 \u s2.

      Lemma union_mem :
        forall a343 : Type,
        forall {a343Ih : Inhab a343},
        forall s : set a343,
        forall s' : set a343,
        forall x : a343,
          Coq.Init.Logic.or (mem x s) (mem x s') -> mem x (union s s').
      Proof.
        intros A Ih s s' x [H | H]; unfold mem, union in *;
          rewrite set_in_union_eq; [left | right]; auto.
      Qed.


      Lemma union_mem_neg :
        forall a350 : Type,
        forall {a350Ih : Inhab a350},
        forall s : set a350,
        forall s' : set a350,
        forall x : a350,
          Coq.Init.Logic.not (mem x s) ->
          Coq.Init.Logic.not (mem x s') -> Coq.Init.Logic.not (mem x (union s s')).
      Proof.
        intros A Ih s s' x H1 H2.
        unfold mem, union in *.
        unfold not. intros H3.
        rewrite set_in_union_eq in H3.
        destruct H3 as [H3 | H3]; contradiction.
      Qed.

      Definition inter {A} {Ih : Inhab A} (s1 : set A) (s2 : set A) : set A := s1 \n s2.

      Lemma inter_mem :
        forall a357 : Type,
        forall {a357Ih : Inhab a357},
        forall s : set a357,
        forall s' : set a357,
        forall x : a357,
          mem x s -> mem x s' -> mem x (inter s s').
      Proof.
        intros A Ih s s' x H1 H2.
        unfold mem, inter in *.
        rewrite set_in_inter_eq. split; auto.
      Qed.


      Lemma inter_mem_neq :
        forall a364 : Type,
        forall {a364Ih : Inhab a364},
        forall s : set a364,
        forall s' : set a364,
        forall x : a364,
          Coq.Init.Logic.not (Coq.Init.Logic.or (mem x s) (mem x s')) ->
          Coq.Init.Logic.not (mem x (inter s s')).
      Proof.
        intros a Ih s s' x H1.
        unfold mem, inter in *.
        rewrite set_in_inter_eq.
        unfold not in *. intros [H2 H3].
        destruct H1. auto.
      Qed.


      Definition disjoint  (a : Type) { aIh : Inhab a } (s : set a) (
          s' : set a
        ) : Prop:=
        Coq.Init.Logic.eq (inter s s') (@empty a aIh).

      Lemma disjoint_def :
        forall {a443 : Type},
        forall {Ih_a443 : Inhab a443},
        forall s : set a443,
        forall s' : set a443,
          Coq.Init.Logic.iff (disjoint s s') (
              Coq.Init.Logic.eq (inter s s') (@empty a443 Ih_a443)
            ).
      Proof.
        tauto.
      Qed.
      
      Definition diff {A} {Ih : Inhab A} (s1 : set A) (s2 : set A) : set A :=
        LibContainer.remove s1 s2.

      Lemma diff_mem :
        forall a373 : Type,
        forall {a373Ih : Inhab a373},
        forall s : set a373,
        forall s' : set a373,
        forall x : a373,
          mem x s' -> Coq.Init.Logic.not (mem x (diff s s')).
      Proof.
        intros A Ih s s' x H1.
        unfold mem, diff in *.
        rewrite set_in_remove_eq. unfold not.
        intros [H2 H3]. contradiction.
      Qed.

      Lemma diff_mem_fst :
        forall a380 : Type,
        forall {a380Ih : Inhab a380},
        forall s : set a380,
        forall s' : set a380,
        forall x : a380,
          Coq.Init.Logic.not (mem x s') ->
          Coq.Init.Logic.iff (mem x s) (mem x (diff s s')).
      Proof.
        intros A Ih s s' x H1.
        unfold mem, diff in *.
        split; intro H2; rewrite set_in_remove_eq in *.
        split; auto.
        - destruct H2 as [H2 H3]. auto.
      Qed.

      Definition subset  (a : Type) { aIh : Inhab a } (s : set a) (s' : set a) : Prop:=
        forall x : a,
          mem x s -> mem x s'.

      
      Lemma subset_def :
        forall {a464 : Type},
        forall {Ih_a464 : Inhab a464},
        forall s : set a464,
        forall s' : set a464,
          Coq.Init.Logic.iff (subset s s') (forall x : a464, mem x s -> mem x s').
      Proof.
        tauto.
      Qed.

      Definition map {A} {B} {Iha : Inhab A} {Ihb : Inhab B} (f : A -> B) (s : set A) : set B :=
        fun (x : B) => exists (y : A), f y = x /\ (y \in s).

      Lemma set_map :
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
                    Coq.Init.Logic.and (eq (f y) x) (mem y s)
                )
            ).
      Proof.
        intros A IhA B IhB f s x.
        unfold mem, map in *. split; auto. 
      Qed.
      
      Definition partition {A} `{Inhab A} p (s : set A) :=
        (set_st (fun x => p x /\ x \in s),
          set_st (fun x => ~p x /\ x \in s)).
      
      Lemma partition_l_mem :
        forall {a486 : Type},
        forall {Ih_a486 : Inhab a486},
        forall f : a486 -> bool,
        forall s : set a486,
        forall x : a486,
        forall p1 : set a486,
        forall p2 : set a486,
          mem x s ->
          f x -> Coq.Init.Logic.eq (partition f s) (p1, p2) -> mem x p1.
      Proof.
        unfold mem, partition.
        intros A IhA f s x p1 p2 H1 H2 H3.
        injection H3.
        intros H4 H5.
        rewrite <- H5.
        rew_set.
        auto.
      Qed.

      Lemma partition_r_mem :
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
      Proof.
        intros A IhA f s x p1 p2 H1 H2 H3.
        injection H3.
        intros H4 H5.
        rewrite <- H4.
        unfold mem.
        rew_set.
        auto.
      Qed.
      
      Definition cardinal {A} {Ih : Inhab A} (s : set A) : Z := Z.of_nat (card s).

      Definition finite  (a : Type) { aIh : Inhab a } (s : set a) : Prop :=
        LibSet.finite s.

      Lemma finite_def :
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
      Proof.
        tauto.
      Qed.

      Lemma cardinal_nonneg :
        forall a399 : Type,
        forall {a399Ih : Inhab a399},
        forall s : set a399,
          ge (cardinal s) (0)%Z.
      Proof.
        intros A Ih s.
        unfold ge, cardinal.
        math.
      Qed.


      Lemma cardinal_empty :
        forall a403 : Type,
        forall {a403Ih : Inhab a403},
          Coq.Init.Logic.eq (cardinal (@empty a403 a403Ih)) (0)%Z.
      Proof.
        intros A IhA.
        unfold cardinal, empty.
        rewrite <- Nat2Z.inj_0.
        f_equal.
        apply card_empty.
      Qed.


      Lemma cardinal_remove :
        forall a415 : Type,
        forall {a415Ih : Inhab a415},
        forall s : set a415,
        forall x : a415,
          finite s ->
          (
            if classicT (mem x s) then
              Coq.Init.Logic.eq (cardinal (remove x s)) (minus (cardinal s) (1)%Z)
            else
              Coq.Init.Logic.eq (cardinal (remove x s)) (cardinal s)
          ).
      Proof.
        intros A Ih s x H.
        unfold mem, cardinal, minus, remove, singleton, empty, add.
        assert (P : x \in s \/ ~ x \in s).
        { apply classic. }
        destruct P.
        - rewrite If_l; auto.
          rewrite card_diff_single; auto.
          + assert (Q: 1%nat <= card s).
            { apply card_ge_one with x; auto. }
            { math. }
        - rewrite If_r; auto.
          f_equal.
          f_equal.
          rewrite set_in_extens_eq.
          intros y. split; intros H1;
            rewrite set_in_remove_eq in *.
          destruct H1 as [H1 H2]. auto.
          + split. auto. unfold not. intro H2.
            rewrite set_in_single_eq in H2.
            subst. contradiction.
      Qed.

      Lemma cardinal_add :
        forall a427 : Type,
        forall {a427Ih : Inhab a427},
        forall s : set a427,
        forall x : a427,
          finite s ->
          (
            if classicT (mem x s) then
              Coq.Init.Logic.eq (cardinal (add x s)) (cardinal s)
            else
              Coq.Init.Logic.eq (cardinal (add x s)) (plus (cardinal s) (1)%Z)
          ).
      Proof.
        intros A Ih s x H.
        unfold plus, cardinal, add in *.
        unfold mem. assert (P : x \in s \/ ~ x \in s). {apply classic. }
        destruct P.
        - rewrite If_l; auto. f_equal. f_equal. rewrite set_in_extens_eq.
          intro y.
          assert (Q:y = x \/ y <> x). { apply classic. }
          split; intro H1; rewrite set_in_union_eq in *.
          + destruct Q. subst. auto.
            destruct H1. auto. contradiction.
          + left. auto.
        - rewrite If_r; auto. rewrite card_disjoint_union_single; auto.
          math.
      Qed.
      
      Definition of_seq {A} {Ih : Inhab A} (s: sequence A) : set A :=
        fun x => LibList.mem x s.

      Lemma of_seq_set :
        forall a433 : Type,
        forall {a433Ih : Inhab a433},
        forall x : a433,
        forall s : sequence a433,
          Coq.Init.Logic.iff (Sequence.mem x s) (mem x (of_seq s)).
      Proof.
        intros A Ih x s.
        unfold mem.
        unfold of_seq.
        split; intros H; auto.
      Qed.

      Lemma of_seq_mem :
        forall {a618 : Type},
        forall {Ih_a618 : Inhab a618},
        forall s : sequence a618,
        forall x : a618,
          Coq.Init.Logic.iff (mem x (of_seq s)) (Sequence.mem x s).
      Proof.
        tauto.
      Qed.


      Definition to_seq {A} `{Inhab A} (s : set A) : sequence A :=
        LibSet.to_list s.

      Lemma count_no_dup : 
        forall A (x : A) l,
          noduplicates l ->
          count (=x) l = 1 <-> LibList.mem x l.
      Proof.
        intros A x l H1.
        split; intros H2.
        - assert (A1 : count (=x) l > 0). { math. }
          apply exists_mem_of_count_pos in A1.
          destruct A1 as [y [H3 H4]]. subst. assumption.
        - induction H1 as [|h l Ih1 Ih2 Ih3].
          + inversion H2.
          + rew_listx in *.
            destruct H2 as [H2 | H2].
            * symmetry in H2. subst. rewrite If_l; auto.
              assert (A2 : ~ count (=x) l > 0).
              { intros H2. rewrite <- Exists_eq_count_pos in H2.
                rewrite Exists_eq_exists_mem in H2.
                destruct H2 as [y [H2 H3]]. subst. contradiction.
              }
              assert (A3 : ~ count (=x) l < 0).
              { unfold count. math. }
              math.
            * rewrite If_r.
              { rewrite Ih3. lia. assumption. }
              { intros H4. subst. contradiction. }
      Qed.

      Lemma to_seq_mem :
        forall {a657 : Type},
        forall {Ih_a657 : Inhab a657},
        forall s : set a657,
          finite s ->
          (
            forall x : a657,
              Coq.Init.Logic.iff (mem x s) (eq (Sequence.multiplicity x (to_seq s)) (1)%Z)
          ).
      Proof.
        intros A Ih s F x.
        unfold to_seq, mem, Sequence.multiplicity, finite in *.
        remember (to_list s) as l eqn:E.
        apply eq_to_list_inv in E; auto.
        unfold list_repr in E.
        destruct E as [E1 E2].
        symmetry.
        rewrite count_no_dup; auto.
      Qed.

      Import LibMonoid.
      Definition fold {A} {B} `{Inhab A} `{Inhab B} (f : A -> B) 
                      (m : B -> B -> B) (s : set A) (acc : B) : B :=
        let monoid := 
          {|
            monoid_oper := m;
            monoid_neutral := acc;
          |} in
        LibContainer.fold monoid f s.

      Lemma fold_def :
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
      Proof.
        unfold comm_monoid. unfold monoid.
        intros A B IhA IhB f m s acc F [[H1 [H2 H3]] H4].
        unfold fold. unfold Sequence.fold_right.
        remember ({|
                      monoid_oper := m;
                      monoid_neutral := acc
                    |}) as op eqn:E.
        rewrite fold_eq_fold_list_repr with
          (A:=A) (B:=B) (m:=op) (f:=f) (E:=s) (L:=to_seq s).
        - induction (to_seq s) as [|h t Ih].
          + rew_listx. subst. auto.
          + rew_listx. rewrite Ih.
            subst. auto.
        - repeat split; rewrite E; auto.
        - unfold to_seq. apply list_repr_to_list_of_finite. auto.
      Qed.

    End _Set.

    Module Map.

      Parameter t : Type -> Type -> Type.

      Definition domain :
        forall {a : Type},
        forall {b : Type},
        forall {_Ga : Inhab a},
        forall {_Gb : Inhab b},
          b -> (a -> b) -> set a.
      Proof.
        refine
          (fun A B G1 G2 d m =>
             set_st (fun x => m x <> d)
          ).
      Defined.

      Lemma domain_mem :
        forall {a545 : Type},
        forall {a546 : Type},
        forall {Ih_a545 : Inhab a545},
        forall {Ih_a546 : Inhab a546},
        forall x : a546,
        forall m : a546 -> a545,
        forall default : a545,
          Coq.Init.Logic.not (eq (m x) default) -> _Set.mem x (domain default m).
      Proof.
        intros A B G1 G2 x m d H1.
        unfold _Set.mem.
        unfold domain.
        rewrite in_set_st_eq.
        auto.
      Qed.
    End Map.

    Lemma map_set_def :
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
    Proof.
      tauto.
    Qed.
    
    Module Array.

      Parameter array : Type -> Type.
      
      Parameter get :
        forall {a : Type},
        forall {_Ga : Inhab a},
          array a -> Coq.Numbers.BinNums.Z -> a.

      Parameter length :
        forall {a : Type},
        forall {_Ga : Inhab a},
          array a -> Coq.Numbers.BinNums.Z.

      Parameter to_seq :
        forall {a : Type},
        forall {_Ga : Inhab a},
          array a -> sequence a.

      Parameter permut :
        forall {a : Type},
        forall {_Ga : Inhab a},
          array a -> array a -> Prop.

      Parameter permut_sub :
        forall {a : Type},
        forall {_Ga : Inhab a},
          array a ->
          array a -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Prop.

    End Array.

    Module List.

      Parameter fold_left :
        forall {a : Type},
        forall {b : Type},
        forall {_Ga : Inhab a},
        forall {_Gb : Inhab b},
          (b -> a -> b) -> b -> list a -> b.

      Parameter _exists :
        forall {a : Type},
        forall {_Ga : Inhab a},
          (a -> Prop) -> list a -> Prop.

      Parameter length :
        forall {a : Type},
        forall {_Ga : Inhab a},
          list a -> Coq.Numbers.BinNums.Z.

      Parameter nth :
        forall {a : Type},
        forall {_Ga : Inhab a},
          list a -> Coq.Numbers.BinNums.Z -> a.

      Parameter mem :
        forall {a : Type},
        forall {_Ga : Inhab a},
          a -> list a -> Prop.

      Parameter map :
        forall {a : Type},
        forall {b : Type},
        forall {_Ga : Inhab a},
        forall {_Gb : Inhab b},
          (a -> b) -> list a -> list b.

    End List.

    Module Order.

      Parameter is_pre_order :
        forall {a : Type},
        forall {_Ga : Inhab a},
          (a -> a -> int) -> Prop.

    End Order.

    Parameter ref : Type -> Type.

    Parameter _UNUSED :
      forall {a : Type},
      forall {_Ga : Inhab a},
        ref a -> a.

    Parameter logand :
      Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z -> Coq.Numbers.BinNums.Z.

    Parameter integer_of_int : int -> Coq.Numbers.BinNums.Z.

    Module Sys.
      Parameter word_size : int.
    End Sys.
  End Gospelstdlib.
End Stdlib.
