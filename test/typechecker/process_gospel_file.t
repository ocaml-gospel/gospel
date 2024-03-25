Testing whether the gospel type-checker save and read the right information in
the `.gospel` file.


  $ cat > foo.mli << EOF
  > type 'a t
  > (*@ ephemeral
  >     mutable model contents : 'a sequence
  >     with x
  >     invariant Sequence.length x.contents > 0 *)
  > 
  > val create : int -> 'a -> 'a t
  > (*@ t = create n a
  >     checks n > 0
  >     ensures t.contents = Sequence.init n (fun _ -> a) *)
  > EOF
  $ gospel check --verbose foo.mli > foo
  $ gospel check --verbose foo.gospel > bar
  $ tail -q -n $(wc -l < bar) foo > baz
  $ diff -s baz bar
  Files baz and bar are identical
