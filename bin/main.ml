open Lib
open Fileio
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
  let sexp = Serializers.sexp_of_dtree dt in
  let sexp2 = Serializers.sexp_of_dtree dt2 in
  (* Verify that the sexp we wrote to a file is the same as the original *)
  assert (Sexplib.Sexp.compare sexp sexp2 = 0) ;
  let incorrect = Classifier.classify_all ~model:m ~examples:ex ~tree:dt in
  Printf.printf "Total Classifications: %d\n" (List.length ex) ;
  Printf.printf "Incorrect Classifications: %d\n" incorrect ;
  let error = ((float_of_int incorrect) /. float_of_int (List.length ex)) in
  Printf.printf "Error Rate: %.2f%%\n" (error *. 100.) ;
  ()