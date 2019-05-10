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

let get_kfold_err exs model depth =
  let rec inner exs' count trainerr valerr = 
    let _ = printf "depth: %d and count: %d\n" depth count in
    match exs' with
    | valset :: rest ->  if count < 4 then
        let _ = printf "valLength: %d and restLength: %d\n" (List.length valset) (List.length rest) in
        let trainset = List.concat rest in
        let dt = make_decision_tree ~examples:trainset ~model:model ~max_depth:(Some depth) in
        let _ = print_tree dt in
        let printlist l = List.iter (fun x -> printf "%s " x) l in
        let _ = List.iter (fun ll -> printlist ll; printf "\n") valset in
        let (t_err, _) = Classifier.classify_all ~model:model ~examples:trainset ~tree:dt in 
        let _ = printf "classified training set" in
        let (v_err, _) = Classifier.classify_all ~model:model ~examples:valset ~tree:dt in
        let _ = printf "Classifier done\n" in
        inner (rest @ [valset]) (count + 1) (trainerr + t_err) (valerr + v_err)
        else let _ = printf "For depth = %d, training error was %d and validation error was %d" trainerr valerr in 
        valerr (*(trainerr, valerr)*)
    | _ -> failwith "Error in get_kfold_err"
  in inner exs 0 0 0
  

let kFold file max_d = 
  let data_file =  (String.concat "" ["data/"; file; ".data"]) in
  (*let tree_file = String.concat "" ["trees/"; file; ".tree"] in*)
  let ex = load_data data_file in
  let m = {
    positive = positive_for file;
    characteristics = characteristics_for file;
    decisions = decisions_for file;
  } in
  let e1, e2, e3, e4 = Helpers.split_in4 ex in
  (*let printlist l = List.iter (fun x -> printf "%s " x) l in
  List.iter (fun ll -> printlist ll) e1; printf "\ne2 = "; 
  List.iter (fun ll -> printlist ll) e2; printf "\ne3 = ";
  List.iter (fun ll -> printlist ll) e3; printf "\ne4 = ";
  List.iter (fun ll -> printlist ll) e4; printf "\ntotal = %d" (List.length ex)*)
  let exs = [e1; e2; e3; e4] in 
  let minerror = ref (get_kfold_err exs m 1) in
  let bestdepth = ref 1 in
  for curr_d = 2 to max_d do
    let curr_error = get_kfold_err exs m curr_d in
    if (!minerror) > curr_error then minerror := curr_error; bestdepth := curr_d 
  done;
  printf "%d " !minerror; printf "%d " !bestdepth
  (*let besttree = make_decision_tree ~examples:ex ~model:m ~max_depth:(Some !bestdepth) in*)
  


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
      else 
      if a1 = "kfold" then 
        let d = int_of_string a3 in
        kFold a2 d
      else failwith "not yet implemented"
  | _ -> failwith fail_msg
