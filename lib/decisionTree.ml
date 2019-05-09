open Model

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

let make_leaf_node depth classification decision examples : LNode.t =
  {
    depth = depth;
    classification = classification;
    decision = decision;
    examples = examples;
  }

let make_split_node depth characteristic decision remainder examples : SNode.t =
  {
    depth = depth;
    characteristic = characteristic;
    decision = decision;
    remainder = remainder;
    examples = examples;
  }


let make_decision_tree ~examples ~model =
  let rec helper used_attrs examples depth decision =
    let (rem, ch) = Helpers.split ~model: model ~examples: examples used_attrs in
    let partitions =
      Helpers.partition examples model.decisions (Helpers.find ch model.characteristics) |>
      List.filter (fun (_, p) -> List.length p <> 0)
    in
    let new_used_attrs = ch :: used_attrs in
    if (rem < 0.0001) || ((List.length new_used_attrs) > (List.length model.characteristics)) then
      let classification = Helpers.get_classification examples model.positive in
      Leaf (make_leaf_node depth classification decision examples)
    else
      Node (make_split_node depth ch decision rem examples, List.map (fun (d, p) -> (helper new_used_attrs p (depth+1) (Some d))) partitions)
  in
  helper [] examples 0 None