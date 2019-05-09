open Serializers


let print_file (file_name: string) =
  let data = Csv.load file_name in
  Csv.print data


let load_data (file_name: string): Csv.t =
  Csv.load file_name


let write_tree dest dt = 
  let oc = open_out dest in
  let sexp = sexp_of_dtree dt in
  let str = Sexplib.Sexp.to_string sexp in
  Printf.fprintf oc "%s" str ;
  close_out oc


let read_tree src =
  let ic = open_in src in
  let s = input_line ic in
  let sexp = Sexplib.Sexp.of_string s in
  let dt = dtree_of_sexp sexp in
  flush stdout ;
  close_in ic ;
  dt