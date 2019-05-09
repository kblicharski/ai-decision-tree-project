open Lib
open Model
open DataModels
open DecisionTree
open Sexplib

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
    positive = positive_for data_file;
    characteristics = characteristics_for data_file;
    decisions = decisions_for data_file;
  } in
  (* let rems = get_all_remainders ~model:m1 ~examples:ex1 in
     print_in_order (List.sort custom_compare rems) ;
     let attr = Helpers.splitting_attr ~model:m1 ~examples:ex1 in
     print_attr attr ; *)
  let dt = make_decision_tree ~examples:ex1 ~model:m1 in
  let sexp = sexp_of_dtree dt in
  Helpers.print_source sexp ;
  (* Make sure that parsing a sexp back into a dtree works *)
  Printf.printf "\n\n\nParsed Tree: \n\n";
  let dt2 = dtree_of_sexp sexp in
  let sexp2 = sexp_of_dtree dt2 in
  Helpers.print_source sexp2 ;
  Printf.printf "\n";
  assert (Sexp.compare sexp sexp2 = 0) ;
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
