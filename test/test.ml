open Fmt

let result_start = "(* EXPECTED"

let print ppf file =
  let ch = open_in file in
  let rec aux ?(first = false) () =
    match input_line ch with
    | s when String.equal result_start s -> ()
    | s ->
        if first then pf ppf "%s" s else pf ppf "@\n%s" s;
        aux ()
    | exception End_of_file -> ()
  in
  aux ~first:true ();
  close_in ch

let print_and_delete ppf file =
  let s = print ppf file in
  Sys.remove file;
  s

let test_file file =
  let stdout = str "%s_stdout" file in
  let stderr = str "%s_stderr" file in
  let command = str "gospel check %s > %s 2> %s" file stdout stderr in
  let status = Sys.command command in
  let output = if status = 0 then stdout else stderr in
  pr "%a@\n%s@\n   [%d] @[%a@]\n*)@\n" print file result_start status
    print_and_delete output

let () =
  let file = Sys.argv.(1) in
  test_file file
