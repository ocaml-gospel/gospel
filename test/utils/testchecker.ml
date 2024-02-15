open Fmt

let result_start = "(* {gospel_expected|"

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

let test_file file =
  let stderr = str "%s_stderr" file in
  let command = str "gospel check %s > %s 2> %s" file Filename.null stderr in
  let status = Sys.command command in
  pr "%a@\n" print file;
  if status <> 0 then (
    pr "(* @[{gospel_expected|@\n[%d] @[%a@]@\n|gospel_expected}@] *)@\n" status
      print stderr;
    let oc = open_out (Filename.remove_extension file ^ ".gospel") in
    close_out oc)

let () =
  let file = Sys.argv.(1) in
  test_file file
