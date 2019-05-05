open Lib

let char1 = [
  "party"; 
  "handicapped-infants"; 
  "adoption-of-the-budget-resolution"; 
  "physician-fee-freeze"; 
  "el-salvador-aid"; 
  "religious-groups-in-schools"; 
  "anti-satellite-test-ban"; 
  "aid-to-nicaraguan-contras"; 
  "mx-missile";
  "immigration";
  "synfuels-corporation-cutback";
  "education-spending";
  "superfund-right-to-sue";
  "crime";
  "duty-free-exports";
  "export-administration-act-south-africa"
]
let decisions1 = ["y"; "n"; "?"]

(* Work in progress, no idea what I'm doing here *)
module DecisionTree =
  struct
    type classification = string
    type node = {
      depth: int;
      parent: node option;
      examples: Csv.t;
      characteristic: string;
    }
  end ;;

let print_partitions partitions labels =
  let rec print_helper p n =
    match p with
    | [] -> ()
    | h :: r -> 
      Printf.printf "Partition '%s'\n" (List.nth labels n) ;
      Csv.print h ; 
      print_helper r (n+1)
  in
  print_helper partitions 0

let () = 
    let data_file = "data/votes-small.data" in
    let examples = Fileio.load_data data_file in
    let branch = 1 in
    let p1 = Helpers.partition examples decisions1 branch in
    Printf.printf "Partitions for '%s' (Index %d)\n" (List.nth char1 branch) branch ;
    print_partitions p1 decisions1 ;
    ()

    (* 
      1. Select a characteristic to branch on. 
      2. Split examples into sets according to their response.
      3. If all examples in that set are homogenous, add a leaf node
         with that classification.
         Otherwise, add new decision nodes containing the set of
         examples belonging to that node, and continue from step 2.
    *)