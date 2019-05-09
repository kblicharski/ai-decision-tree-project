open Lib
open Fileio
open Helpers
open Model
open DataModels
open DecisionTree

let () =
  let data_file = "data/house-votes-84.data" in
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