open Model


let print_partitions partitions labels =
  let rec print_helper p n =
    match p with
    | [] -> ()
    | (_, h) :: r ->
      Printf.printf "Partition '%s'\n" (List.nth labels n) ;
      Csv.print h ;
      print_helper r (n+1)
  in
  print_helper partitions 0


(* let make_decision_tree ~examples ~(characteristics: (string * string list) list) ~max_depth = *)
let make_decision_tree ~examples ~characteristics ~max_depth =
  let make_leaf_node depth classification decision examples : LNode.t =
    {
      depth = depth;
      classification = classification;
      decision = decision;
      examples = examples;
    }
  in
  let make_split_node depth characteristic decision remainder examples : SNode.t =
    {
      depth = depth;
      characteristic = characteristic;
      decision = decision;
      remainder = remainder;
      examples = examples;
    }
  in
  let rec helper used_attrs examples depth decision =
    let all_names = (List.map (fun (name, _) -> name) characteristics) in
    let (rem, ch) = Helpers.split ~characteristics:characteristics ~examples:examples used_attrs in
    let partitions =
      let index = (Helpers.find ch all_names) in
      let (_, decisions) = List.nth characteristics index in
      Helpers.partition examples decisions index |>
      List.filter (fun (_, p) -> List.length p <> 0)
    in
    let new_used_attrs = ch :: used_attrs in
    let should_generate_leaves =
      let out_of_attrs attrs = ((List.length attrs) >= (List.length characteristics) - 1) in
      match max_depth with
      | Some d -> ((depth+1) = d) || not (rem > 0.00) || (out_of_attrs new_used_attrs)
      | None -> not (rem > 0.00) || (out_of_attrs new_used_attrs)
    in
    if should_generate_leaves then
      Node (make_split_node depth ch decision rem examples, List.map (
        fun (d, e) ->
          let classification = Helpers.get_classification e in
          Leaf (make_leaf_node (depth+1) classification (Some d) e)
      ) partitions
      )
    else
      Node (make_split_node depth ch decision rem examples, List.map (fun (d, p) -> (helper new_used_attrs p (depth+1) (Some d))) partitions)
  in
  helper [] examples 0 None

let print_tree dt =
  let print_source ?(channel = stdout) sexp =
    let formatter = Format.formatter_of_out_channel channel in
    Sexplib.Sexp.pp_hum formatter sexp;
    Printf.printf "\n\n";
    Format.pp_print_flush formatter ()
  in
  let sexp = Serializers.sexp_of_dtree dt in
  print_source sexp ;
