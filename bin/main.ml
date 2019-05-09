open Lib
open Fileio
open DataModels
open DecisionTree
open Printf


let createClassifier file =
  let data_file =  (String.concat "" ["data/"; file; ".data"]) in
  let ex = load_data data_file in
  let characteristics = characteristics_for file in
  let dt = make_decision_tree ~examples:ex characteristics in
  print_tree dt ;
  let model_file = String.concat "" ["trees/"; file; ".tree"] in
  write_tree model_file dt ;
  let dt2 = read_tree model_file in
  let sexp = Serializers.sexp_of_dtree dt in
  let sexp2 = Serializers.sexp_of_dtree dt2 in
  (* Verify that the sexp we wrote to a file is the same as the original *)
  assert (Sexplib.Sexp.compare sexp sexp2 = 0) ;

  let (incorrect, i_exs) = Classifier.classify_all ~examples:ex ~tree:dt2 ~characteristics:characteristics in
  printf "Total Classifications: %d\n" (List.length ex) ;
  printf "Incorrect Classifications: %d\n" incorrect ;
  let error = ((float_of_int incorrect) /. float_of_int (List.length ex)) in
  printf "Error Rate: %.2f%%\n" (error *. 100.) ;
  printf "\nIncorrectly Classified:\n" ;
  List.iter (fun e -> List.iter (printf "%s ") e; printf "\n") i_exs ;
  printf "\n\n" ;
  ()


let () =
  match ((Array.length Sys.argv) - 1) with
  | 0 ->
    printf "This program can be used with the following arguments:
    tree [dataFile]\n" (* add more usages later *)
  | 2 ->
    if (Array.get Sys.argv 1) = "tree" then
      createClassifier (Array.get Sys.argv 2)
    else failwith "not yet implemented"
  | _ -> failwith "The given args do not fit any possible usages. To see program usages, execute without any arguments."
