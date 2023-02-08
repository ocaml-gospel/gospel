type t

(*@ type t *)

(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 Sys_error(": No such file or directory")
                 Raised by primitive operation at Stdlib.open_in_gen in file "stdlib.ml", line 403, characters 28-54
                 Called from Stdlib.open_in_bin in file "stdlib.ml" (inlined), line 411, characters 2-47
                 Called from Pp_loc.Input.file.(fun) in file "lib/pp_loc.ml", line 73, characters 16-32
                 Called from Pp_loc.Input.open_raw in file "lib/pp_loc.ml", line 107, characters 17-21
                 Called from Pp_loc.Position.to_lexing.(fun) in file "lib/pp_loc.ml", line 161, characters 23-43
                 Called from Gospel__Warnings.pp in file "src/warnings.ml", line 169, characters 54-76
                 Called from Stdlib__Format.output_acc in file "format.ml", line 1360, characters 4-20
                 Called from Dune__exe__Check.run_file in file "bin/check.ml", line 58, characters 4-25
                 Called from Dune__exe__Check.run.(fun) in file "bin/check.ml", line 62, characters 33-53
                 Called from Dune__exe__Cli.run_check in file "bin/cli.ml", line 42, characters 10-47
                 Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
                 Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
   |gospel_expected} *)
