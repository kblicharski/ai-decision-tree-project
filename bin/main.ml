open Lib
open Fileio
open DataModels
open DecisionTree
open Printf




let classify file =
  let data_src =  (String.concat "" ["data/"; file; ".data"]) in
  let tree_src = String.concat "" ["trees/"; file; ".tree"] in
  let ex = load_data data_src in
  let dt = read_tree tree_src in
  let characteristics = characteristics_for file in
  let (incorrect, i_exs) = Classifier.classify_all ~examples:ex ~tree:dt ~characteristics:characteristics in
  printf "\nTotal Classifications: %d\n" (List.length ex) ;
  printf "Incorrect Classifications: %d\n" incorrect ;
  let error = ((float_of_int incorrect) /. float_of_int (List.length ex)) in
  printf "Error Rate: %.2f%%\n" (error *. 100.) ;
  printf "\nIncorrectly Classified:\n" ;
  List.iter (fun e -> List.iter (printf "%s ") e; printf "\n") i_exs ;
  printf "\n" ;
  ()


let createClassifier file max_d =
  let data_file =  (String.concat "" ["data/"; file; ".data"]) in
  let tree_file = String.concat "" ["trees/"; file; ".tree"] in
  let ex = load_data data_file in
  let characteristics = characteristics_for file in
  let dt = make_decision_tree ~examples:ex ~characteristics:characteristics ~max_depth:max_d in
  print_tree dt ;
  write_tree tree_file dt ;
  ()


let () =
  let fail_msg = "The given args do not fit any possible usages. To see program usages, execute without any arguments." in
  match ((Array.length Sys.argv) - 1) with
  | 0 ->
    printf "This program can be used with the following arguments:
    dtl [dataFile]\n
    dtl [dataFile] [maxDepth]\n
    classify [dataFile]\n
    " (* add more usages later *)
  | 2 ->
      let a1 = (Array.get Sys.argv 1) in
      let a2 = (Array.get Sys.argv 2) in
      if a1 = "dtl" then
        createClassifier a2 None
      else if a1 = "classify" then
        classify a2
      else
        failwith "not yet implemented"
  | 3 ->
      let a1 = (Array.get Sys.argv 1) in
      let a2 = (Array.get Sys.argv 2) in
      let a3 = (Array.get Sys.argv 3) in
      if a1 = "dtl" then
        let d = int_of_string_opt a3 in
        createClassifier a2 d
      else failwith "not yet implemented"
  | _ -> failwith fail_msg
