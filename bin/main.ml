open Lib

let char1 = [
  "party"; 
  "handicapped-infants"; 
  "water-project-cost-sharing";
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
let positive1 = "democrat"


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


let get_all_remainders examples decisions characteristics pos =
  let rec r_helper chars o =
    match chars with
    | [] -> o
    | h :: r -> 
      let i = Helpers.find h characteristics in
      let p = Helpers.partition examples decisions i in
      let rem = Helpers.remainder examples p pos in
      r_helper r ((rem, List.nth characteristics i) :: o)
  in
  r_helper (List.tl characteristics) []


let rec print_in_order rems =
  match rems with
  | [] -> ()
  | (rem, c) :: r -> 
    Printf.printf "%f -- %s\n" rem c ;
    print_in_order r

let custom_compare (v1, _) (v2, _) =
  if v1 = v2 then 0 else
    if v1 > v2 then 1 else -1

let () = 
    (* let data_file = "data/votes-small.data" in *)
    let data_file = "data/house-votes-84.data" in
    let examples = Fileio.load_data data_file in
    (* let branch = 1 in *)
    let positive = positive1 in
    let chars = char1 in
    let decisions = decisions1 in
    (* let p = Helpers.partition examples decisions branch in *)
    (* Printf.printf "Partitions for '%s' (Index %d)\n" (List.nth chars branch) branch ; *)
    (* print_partitions p decisions ; *)
    (* Printf.printf "Remainder for '%s': %f\n" (List.nth chars branch) (Helpers.remainder examples p positive) ; *)
    let rems = get_all_remainders examples decisions chars positive in
    print_in_order (List.sort custom_compare rems) ;
    ()

    (* 
      1. Select a characteristic to branch on. 
      2. Split examples into sets according to their response.
      3. If all examples in that set are homogenous, add a leaf node
         with that classification.
         Otherwise, add new decision nodes containing the set of
         examples belonging to that node, and continue from step 2.
    *)