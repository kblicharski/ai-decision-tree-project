let read_file (file_name: string) =
  let data = Csv.load file_name in
  Csv.print data
(* let in_channel = open_in file_name in
   try
   while true do
    let line = input_line in_channel in
    print_endline line
   done
   with End_of_file ->
   close_in in_channel *)

let load_data (file_name: string): Csv.t =
  Csv.load file_name


let write_tree (dest: string) (sexp: Sexplib.Sexp.t) = 
  let oc = open_out dest in
  let str = Sexplib.Sexp.to_string sexp in
  Printf.fprintf oc "%s" str ;
  close_out oc

let read_tree (src: string) =
  let ic = open_in src in
  let s = input_line ic in
  let sexp = Sexplib.Sexp.of_string s in
  let dt = Model.dtree_of_sexp sexp in
  flush stdout ;
  close_in ic ;
  dt