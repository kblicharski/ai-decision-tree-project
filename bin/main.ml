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

let get_kfold_err exs characteristics depth =
  let rec inner exs' count trainerr valerr = 
    match exs' with
    | valset :: rest ->  if count < 4 then
        let trainset = List.concat rest in
        let dt = make_decision_tree ~examples:trainset ~characteristics:characteristics ~max_depth:(Some depth) in
        let (t_err, _) = Classifier.classify_all ~examples:trainset ~tree:dt ~characteristics:characteristics in 
        let (v_err, _) = Classifier.classify_all ~examples:valset ~tree:dt ~characteristics:characteristics in 
        inner (rest @ [valset]) (count + 1) (trainerr + t_err) (valerr + v_err)
        else 
        (trainerr, valerr)
    | _ -> failwith "Error in get_kfold_err"
  in 
  let terr, verr = inner exs 0 0 0 in
  let total = List.length (List.concat exs) in
  let t_percent = (float_of_int (terr)) /. float_of_int(total * 3) in
  let v_percent = (float_of_int (verr)) /. float_of_int(total) in
  let _ = printf "For depth = %d, average training error was %.4f and validation error was %.4f\n" depth (t_percent) (v_percent) in
  verr
  

let kFold file max_d = 
  let data_file =  (String.concat "" ["data/"; file; ".data"]) in
  let ex = load_data data_file in
  let characteristics = characteristics_for file in
  let e1, e2, e3, e4 = Helpers.split_in4 ex in
  let exs = [e1; e2; e3; e4] in 
  let minerror = ref (get_kfold_err exs characteristics 1) in
  let bestdepth = ref 1 in
  for curr_d = 2 to max_d do
    let curr_error = get_kfold_err exs characteristics curr_d in
    if (!minerror) > curr_error then (minerror := curr_error; bestdepth := curr_d)
  done;
  printf "The minimum average validation error was %.4f at a depth of %d\n" ((float_of_int !minerror)/.(float_of_int (List.length ex))) !bestdepth;
  let besttree = make_decision_tree ~examples:ex ~characteristics:characteristics ~max_depth:(Some !bestdepth) in
  let (error, _) = Classifier.classify_all ~examples:ex ~tree:besttree ~characteristics in
  printf "Training error for final tree of depth %d is %.4f\n" !bestdepth ((float_of_int error)/.(float_of_int (List.length ex))); 
  ()
  


let () =
  let fail_msg = "The given args do not fit any possible usages. To see program usages, execute without any arguments." in
  match ((Array.length Sys.argv) - 1) with
  | 0 ->
    printf "This program can be used with the following arguments:
    dtl [dataFile]\n
    dtl [dataFile] [maxDepth]\n
    classify [dataFile]\n
    kfold [dataFile] [maxDepth]
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
      else 
      if a1 = "kfold" then 
        let d = int_of_string a3 in
        kFold a2 d
      else failwith "not yet implemented"
  | _ -> failwith fail_msg
