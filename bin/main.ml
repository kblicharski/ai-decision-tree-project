open Lib
open Fileio
open Helpers
open Model
open DataModels
open DecisionTree

let createClassifier file =
  (* let data_file = "data/votes-small.data" in *)
  (* let data_file = "data/house-votes-84.data" in *)
  let data_file =  (String.concat "" ["data/"; file]) in
  let ex = load_data data_file in
  let m = {
    positive = positive_for data_file;
    characteristics = characteristics_for data_file;
    decisions = decisions_for data_file;
  } in
  let dt = make_decision_tree ~examples:ex ~model:m in
  print_tree dt ;
  write_tree "trees/house-votes-84.tree" dt ;
  let dt2 = read_tree "trees/house-votes-84.tree" in
  let sexp = sexp_of_dtree dt in
  let sexp2 = sexp_of_dtree dt2 in
  (* Verify that the sexp we wrote to a file is the same as the original *)
  assert (Sexplib.Sexp.compare sexp sexp2 = 0) ;
  ()


let () =
  match ((Array.length Sys.argv) - 1) with
  | 0 -> 
    Printf.printf "This program can be used with the following arguments: 
    tree [dataFile]\n" (* add more usages later *)
  | 2 ->
    if (Array.get Sys.argv 1) = "tree" then
      createClassifier (Array.get Sys.argv 2)
      else failwith "not yet implemented"
  | _ -> failwith "The given args do not fit any possible usages. To see program usages, execute without any arguments."