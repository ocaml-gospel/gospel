Running `gospel check --verbose` to test `T.module.print_file` (calling
`Tast_printer.print_signature`, calling `TTerm_printer.print_term`)

  $ gospel check --verbose lib.mli | awk '/Signatures/,/EOF/ { if (match($1, /Signatures/) == 0) print }'
      (*@ open Stdlib *)
      
      (*@ open Gospelstdlib *)
      
      (*@ axiom prefix: exists x:integer -> integer. (x:integer -> integer = ((-) 42:
      integer):integer -> integer):prop *)
      
      (*@ axiom infix: exists x_1:integer y:integer. ((x_1:integer - y:integer):
      integer = 0:integer):prop *)
      
      (*@ axiom infix_partial_application: exists x_2:integer y_1:integer. let 
      f:integer -> integer = ((-) x_2:integer):integer -> integer in ((apply 
      f:integer -> integer y_1:integer):integer = 0:integer):prop *)
      
      (*@ axiom mixfix: exists xs:integer sequence. ( = 42:integer):prop *)
      
      (*@ axiom mixfix_partial_application: exists xs_1:integer sequence. let 
      f_1:integer -> integer sequence =  in (mem 
      (apply  f_1:integer -> integer sequence 73:integer):integer sequence
      42:integer):prop *)
  

  $ gospel check --verbose ../../src/stdlib/gospelstdlib.mli | awk '/Signatures/,/EOF/ { if (match($1, /\(\*@/) != 0) print }'
      (*@ type 'a sequence
      (*@ type 'a bag
      (*@ type 'a ref
      (*@ type 'a set
      (*@ function succ (x:integer): integer *)
      (*@ function pred (x_1:integer): integer *)
      (*@ function -_1 (x_2:integer): integer *)
      (*@ function + (x_3:integer) (y:integer): integer *)
      (*@ function - (x_4:integer) (y_1:integer): integer *)
      (*@ function * (x_5:integer) (y_2:integer): integer *)
      (*@ function / (x_6:integer) (y_3:integer): integer *)
      (*@ function mod (x_7:integer) (y_4:integer): integer *)
      (*@ function pow (x_8:integer) (y_5:integer): integer *)
      (*@ function abs (x_9:integer): integer *)
      (*@ function min (x_10:integer) (y_6:integer): integer *)
      (*@ function max (x_11:integer) (y_7:integer): integer *)
      (*@ predicate > (x_12:integer) (y_8:integer) *)
      (*@ predicate >= (x_13:integer) (y_9:integer) *)
      (*@ predicate < (x_14:integer) (y_10:integer) *)
      (*@ predicate <= (x_15:integer) (y_11:integer) *)
      (*@ function logand (x_16:integer) (y_12:integer): integer *)
      (*@ function logor (x_17:integer) (y_13:integer): integer *)
      (*@ function logxor (x_18:integer) (y_14:integer): integer *)
      (*@ function lognot (x_19:integer): integer *)
      (*@ function shift_left (x_20:integer) (y_15:integer): integer *)
      (*@ function shift_right (x_21:integer) (y_16:integer): integer *)
      (*@ function shift_right_trunc (x_22:integer) (y_17:integer): integer *)
      (*@ function integer_of_int (x_23:int): integer
      (*@ function max_int : integer *)
      (*@ function min_int : integer *)
      (*@ function fst (p:'a * 'b): 'a *)
      (*@ function snd (p_1:'a * 'b): 'b *)
      (*@ function ! (r:'a ref): 'a *)
      (*@ function ++ (s:'a sequence) (s':'a sequence): 'a sequence *)
      (*@ function [_] (s_1:'a sequence) (i:integer): 'a *)
      (*@ function [_.._] (s_2:'a sequence) (i1:integer) (i2:integer): 'a 
      (*@ function [_..] (s_3:'a sequence) (i_1:integer): 'a sequence *)
      (*@ function [.._] (s_4:'a sequence) (i_2:integer): 'a sequence *)
        (*@ type 'a t_3 = 'a sequence
        (*@ function length_2 (s_5:'a sequence): integer *)
        (*@ function empty_1 : 'a sequence *)
        (*@ function singleton_1 (x_24:'a): 'a sequence *)
        (*@ function init_2 (n:integer) (f:integer -> 'a): 'a sequence *)
        (*@ function cons (x_25:'a) (s_6:'a sequence): 'a sequence *)
        (*@ function snoc (s_7:'a sequence) (x_26:'a): 'a sequence *)
        (*@ function hd_1 (s_8:'a sequence): 'a *)
        (*@ function tl_1 (s_9:'a sequence): 'a sequence *)
        (*@ function append_1 (s_10:'a sequence) (s'_1:'a sequence): 'a 
        (*@ predicate mem_3 (s_11:'a sequence) (x_27:'a) *)
        (*@ function map_3 (f_1:'a -> 'b) (s_12:'a sequence): 'b sequence *)
        (*@ function filter_1 (f_2:'a -> bool) (s_13:'a sequence): 'a sequence
        (*@ function filter_map_1 (f_3:'a -> 'b option) (s_14:'a sequence): 'b 
        (*@ function get_1 (s_15:'a sequence) (i_3:integer): 'a *)
        (*@ function set_1 (s_16:'a sequence) (i_4:integer) (x_28:'a): 'a 
        (*@ function rev_1 (s_17:'a sequence): 'a sequence *)
        (*@ function rec fold_left_2 (f_4:'a -> 'b -> 'a) (acc:'a)
        (*@ function rec fold_right_2 (f_5:'a -> 'b -> 'b) (s_19:'a sequence)
        (*@ type 'a t_2 = 'a list
        (*@ function length_1 (l:'a list): integer *)
        (*@ function hd (l_1:'a list): 'a *)
        (*@ function tl (l_2:'a list): 'a list *)
        (*@ function nth (l_3:'a list) (i_5:integer): 'a *)
        (*@ function nth_opt (l_4:'a list) (i_6:integer): 'a option *)
        (*@ function rev (l_5:'a list): 'a list *)
        (*@ function init_1 (n_1:integer) (f_6:integer -> 'a): 'a list *)
        (*@ function map_2 (f_7:'a -> 'b) (l_6:'a list): 'b list *)
        (*@ function mapi_1 (f_8:integer -> 'a -> 'b) (l_7:'a list): 'b list *)
        (*@ function fold_left_1 (f_9:'a -> 'b -> 'a) (init_3:'a) (l_8:'b list): 'a
        (*@ function fold_right_1 (f_10:'b -> 'a -> 'a) (l_9:'b list)
        (*@ function map2_1 (f_11:'a -> 'b -> 'c) (l_10:'a list) (l':'b list): 'c 
        (*@ predicate for_all_2 (f_12:'a -> bool) (l_11:'a list) *)
        (*@ predicate _exists_2 (f_13:'a -> bool) (l_12:'a list) *)
        (*@ predicate for_all2_1 (f_14:'a -> 'b -> bool) (l_13:'a list)
        (*@ predicate _exists2_1 (f_15:'a -> 'b -> bool) (l_14:'a list)
        (*@ predicate mem_2 (x_29:'a) (l_15:'a list) *)
        (*@ function to_seq_2 (s_20:'a list): 'a sequence
        (*@ function of_seq_2 (s_21:'a sequence): 'a list *)
        (*@ type 'a t = 'a array
        (*@ function length (a_1:'a array): integer *)
        (*@ function get (a_2:'a array) (i_7:integer): 'a *)
        (*@ function make (n_2:integer) (x_30:'a): 'a array *)
        (*@ function init (n_3:integer) (f_16:integer -> 'a): 'a array *)
        (*@ function append (a_3:'a array) (b_1:'a array): 'a array *)
        (*@ function concat (a_4:'a array list): 'a array *)
        (*@ function sub (a_5:'a array) (i_8:integer) (len:integer): 'a array
        (*@ function map (f_17:'a -> 'b) (a_6:'a array): 'b array *)
        (*@ function mapi (f_18:integer -> 'a -> 'b) (a_7:'a array): 'b array
        (*@ function fold_left (f_19:'a -> 'b -> 'a) (init_5:'a) (a_8:'b array): 'a
        (*@ function fold_right (f_20:'b -> 'a -> 'a) (a_9:'b array)
        (*@ function map2 (f_21:'a -> 'b -> 'c) (a_10:'a array) (b_2:'b array): 'c 
        (*@ predicate for_all (f_22:'a -> bool) (a_11:'a array) *)
        (*@ predicate _exists (f_23:'a -> bool) (a_12:'a array) *)
        (*@ predicate for_all2 (f_24:'a -> 'b -> bool) (a_13:'a array)
        (*@ predicate _exists2 (f_25:'a -> 'b -> bool) (a_14:'a array)
        (*@ predicate mem (x_31:'a) (a_15:'a array) *)
        (*@ function to_list (a_16:'a array): 'a list *)
        (*@ function of_list (l_16:'a list): 'a array *)
        (*@ function to_seq (a_17:'a array): 'a sequence
        (*@ function of_seq (s_22:'a sequence): 'a array *)
        (*@ function to_bag (a_18:'a array): 'a bag *)
        (*@ predicate permut (a_19:'a array) (b_5:'a array) *)
        (*@ predicate permut_sub (a_20:'a array) (b_6:'a array) (lo:integer)
        (*@ type 'a t_1 = 'a bag
        (*@ function occurrences (x_32:'a) (b_7:'a bag): integer *)
        (*@ function empty : 'a bag *)
        (*@ predicate is_empty (b_8:'a bag) *)
        (*@ predicate mem_1 (x_33:'a) (b_9:'a bag) *)
        (*@ function add (x_34:'a) (b_10:'a bag): 'a bag *)
        (*@ function singleton (x_35:'a): 'a bag *)
        (*@ function remove (x_36:'a) (b_11:'a bag): 'a bag *)
        (*@ function union (b_12:'a bag) (b':'a bag): 'a bag *)
        (*@ function sum (b_13:'a bag) (b'_1:'a bag): 'a bag *)
        (*@ function inter (b_14:'a bag) (b'_2:'a bag): 'a bag *)
        (*@ predicate disjoint (b_15:'a bag) (b'_3:'a bag) *)
        (*@ function diff (b_16:'a bag) (b'_4:'a bag): 'a bag *)
        (*@ predicate subset (b_17:'a bag) (b'_5:'a bag) *)
        (*@ function choose (b_18:'a bag): 'a *)
        (*@ function choose_opt (b_19:'a bag): 'a option *)
        (*@ function map_1 (f_26:'a -> 'b) (b_20:'a bag): 'b bag *)
        (*@ function fold (f_27:'a -> 'b -> 'b) (b_21:'a bag) (a_21:'b): 'b *)
        (*@ predicate for_all_1 (f_28:'a -> bool) (b_22:'a bag) *)
        (*@ predicate _exists_1 (f_29:'a -> bool) (b_23:'a bag) *)
        (*@ function filter (f_30:'a -> bool) (b_24:'a bag): 'a bag *)
        (*@ function filter_map (f_31:'a -> 'a option) (b_25:'a bag): 'a bag *)
        (*@ function partition (f_32:'a -> bool) (b_26:'a bag): 'a bag * 'a bag
        (*@ function cardinal (b_27:'a bag): integer *)
        (*@ function to_list_1 (b_28:'a bag): 'a list *)
        (*@ function of_list_1 (l_17:'a list): 'a bag *)
        (*@ function to_seq_1 (b_29:'a bag): 'a sequence *)
        (*@ function of_seq_1 (s_23:'a sequence): 'a bag *)
      (*@ function {} : 'a set *)
        (*@ type 'a t_4 = 'a set
        (*@ function compare (s_24:'a set) (s'_2:'a set): integer *)
        (*@ function empty_2 : 'a set *)
        (*@ predicate is_empty_1 (s_25:'a set) *)
        (*@ predicate mem_4 (x_37:'a) (s_26:'a set) *)
        (*@ function add_1 (x_38:'a) (s_27:'a set): 'a set *)
        (*@ function singleton_2 (x_39:'a): 'a set *)
        (*@ function remove_1 (x_40:'a) (s_28:'a set): 'a set *)
        (*@ function union_1 (s_29:'a set) (s'_3:'a set): 'a set *)
        (*@ function inter_1 (s_30:'a set) (s'_4:'a set): 'a set *)
        (*@ predicate disjoint_1 (s_31:'a set) (s'_5:'a set) *)
        (*@ function diff_1 (s_32:'a set) (s'_6:'a set): 'a set *)
        (*@ predicate subset_1 (s_33:'a set) (s'_7:'a set) *)
        (*@ function cardinal_1 (s_34:'a set): integer *)
        (*@ function choose_1 (s_35:'a set): integer *)
        (*@ function choose_opt_1 : 'a set -> 'a option *)
        (*@ function map_4 (f_33:'a -> 'b) (s_36:'a set): 'b set *)
        (*@ function fold_1 (f_34:'a -> 'b -> 'b) (s_37:'a set) (a_22:'b): 'b
        (*@ predicate for_all_3 (f_35:'a -> bool) (s_38:'a set) *)
        (*@ predicate _exists_3 (f_36:'a -> bool) (s_39:'a set) *)
        (*@ function filter_2 (f_37:'a -> bool) (s_40:'a set): 'a set *)
        (*@ function filter_map_2 (f_38:'a -> 'a option) (s_41:'a set): 'a set
        (*@ function partition_1 (f_39:'a -> bool) (s_42:'a set): 'a set *
        (*@ function to_list_2 (s_43:'a set): 'a list *)
        (*@ function of_list_2 (l_18:'a list): 'a set *)
        (*@ function to_seq_3 (s_44:'a set): 'a sequence *)
        (*@ function of_seq_3 (s_45:'a sequence): 'a set *)
      (*@ function [->] (f_40:'a -> 'b) (x_41:'a) (y_18:'b): 'a -> 'b *)
        (*@ predicate is_pre_order (cmp:'a -> 'a -> int) =
        (*@ function word_size : integer *)
        (*@ function int_size : integer *)
        (*@ function big_endian : bool *)
        (*@ function max_string_length : integer *)
        (*@ function max_array_length : integer *)
