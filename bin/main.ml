open Lib
open Model

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


let get_all_remainders ~model ~examples =
  let rec r_helper chars o =
    match chars with
    | [] -> o
    | h :: r ->
      let i = Helpers.find h model.characteristics in
      let p = Helpers.partition examples model.decisions i in
      let rem = Helpers.remainder examples p model.positive in
      r_helper r ((rem, List.nth model.characteristics i) :: o)
  in
  r_helper (List.tl model.characteristics) []


let rec print_in_order rems =
  match rems with
  | [] -> ()
  | (rem, c) :: r ->
    Printf.printf "%f -- %s\n" rem c ;
    print_in_order r


let custom_compare (v1, _) (v2, _) =
  if v1 = v2 then 0 else
    if v1 > v2 then 1 else -1

let print_attr a =
  match a with
  | (f, s) -> Printf.printf "\n\n%f -- %s\n" f s

let () =
    (* let data_file = "data/votes-small.data" in *)
    let data_file = "data/house-votes-84.data" in
    let ex1 = Fileio.load_data data_file in
    let m1 = { 
      positive = positive1; 
      characteristics = char1; 
      decisions = decisions1
    } in
    let rems = get_all_remainders ~model:m1 ~examples:ex1 in
    print_in_order (List.sort custom_compare rems) ;
    let attr = Helpers.splitting_attr ~model:m1 ~examples:ex1 in
    print_attr attr ;
    ()

    (* 
      1. Select a characteristic to branch on. 
         (a) Compute all remainders using `get_all_remainders`
         (b) Select the characteristic with the minimum entropy value
      2. Split examples into sets according to their response.
         (a) Call `partition` with the current set of examples, the set
             of available decisions (branching factor), and the index in
             the characteristics array of the characteristic being
             used to split.
      3. Create a new Node object to store the returned information.
         (a) 
         Otherwise, add new decision nodes containing the set of
         examples belonging to that node, and repeat.
    *)