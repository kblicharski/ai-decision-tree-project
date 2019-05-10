let generate_range d =
  let rec helper i o =
    match i with
    | 0 -> o
    | _ -> helper (i-1) (i :: o)
  in
  helper d []

(* let dts = List.map (fun i -> make_decision_tree ~examples:ex ~characteristics:characteristics ~max_depth:i) range *)


let find x l =
  let rec find_helper x l n =
    match l with
    | [] -> failwith "Item not found in list"
    | h :: r -> if h = x then n else find_helper x r (n+1)
  in
  find_helper x l 0



let get_option = function
  | Some x -> x
  | None -> ""


(*
  Returns all unique values in a list.

  Inputs:
    (l: string list)
      The input list.

  Output:
    (string list)
      A list with duplicates removed. Unique values are
      returned in the order they are encountered in the original.

  Examples:
    # uniq ["y"; "?"; "y"; "n"; "?"; "y"]
    - : string list = ["n"; "?"; "y"]
*)
let uniq l =
  let rec uniq_h l o =
    match l with
    | [] -> o
    | h :: r ->
      if List.mem h o then
        uniq_h r o
      else
        uniq_h r (h :: o)
  in
  List.rev (uniq_h l [])

(*
  Partitions a set of examples into n example sets,
  where n is the number of possible decisions.

  Inputs:
    (e: string list list)
      The set of examples to partition.
    (u: string list)
      The set of decisions. Note that the order of the decisions in u
       will be the order of the matching in the output. For example,
      if u = ["y", "n", "?"] then the first element in the output will
       be the set of examples that matched "y".
    (c: int)
      The characteristic to partition against. This value is
      used to index into each individual example.

  Output:
    (string list list list)
      The partitioned sets of examples.

  Examples:
    # partition [["y"; "n"]; ["y"; "?"]] ["y"; "n"; "?"] 0
    - : string list list list = [[["y"; "n"]; ["y"; "?"]]; []; []]

    # partition [["y"; "n"]; ["y"; "?"]] ["y"; "n"; "?"] 1
    - : string list list list = [[]; [["y"; "n"]]; [["y"; "?"]]]
*)
let partition e u c =
  let rec partition_uniq l o =
    match l with
    | [] -> o
    | h :: r ->
      let matched = List.filter (fun x -> (List.nth x c) = h) e in
      partition_uniq r ((h, matched) :: o)
  in
  List.rev (partition_uniq u [])


let remainder examples partitioned pos =
  let entropy p n =
    let log2 v = Pervasives.log10(v) /. Pervasives.log10(2.) in
    let z = p +. n in
    let a = if p > 0.00 then -.(p /. z) *. (log2 (p /. z)) else 0. in
    let b = if n > 0.00 then -.(n /. z) *. (log2 (n /. z)) else 0. in
    a +. b
  in
  let find_p_n ex =
    let (p_, n_) = List.partition (fun e -> (List.nth e 0) = pos) ex in
    (float_of_int (List.length p_), float_of_int (List.length n_))
  in
  let (p, n) = find_p_n examples in
  let pis_nis = List.map find_p_n partitioned in
  let calc_remainder (pi, ni) =
    let z = pi +. ni in
    let rem = (z /. (p +. n)) *. (entropy (pi /. z) (ni /. z)) in
    rem
  in
  let remainders = List.map calc_remainder pis_nis in
  let sum l = List.fold_right (+.) l 0. in
  sum remainders


let get_classification examples  =
  let group_by_class lst =
    let cmp_class e1 e2 = List.hd e1 = List.hd e2 in
    let rec group_helper acc = function
      | [] -> acc
      | h::t -> let _, l2 =
                  List.partition (cmp_class h) t in
        group_helper ((h::t)::acc) l2
    in
    group_helper [] lst
  in
  let groups = group_by_class examples in
  let plurality = List.sort (fun a b -> List.length b - List.length a) groups in
  List.hd (List.hd (List.hd plurality))

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

let get_all_remainders ~characteristics ~examples =
  let all_names = (List.map (fun (name, _) -> name) characteristics) in
  let positive = (get_classification examples) in
  let rec r_helper chars o =
    match chars with
    | [] -> o
    | (char_name, decisions) :: r ->
      let i = find char_name all_names in
      (* let () = Printf.printf "%s -- %d\n" char_name i in
      let () = List.iter (fun e -> List.iter (Printf.printf "%s ") e; Printf.printf "\n") examples in
      let () = Printf.printf "\n" in *)
      let p_and_d = partition examples decisions i in
      let p = List.map (fun (_, p) -> p) p_and_d in
      (* let () = print_partitions p_and_d decisions in *)
      let rem = remainder examples p positive in
      r_helper r ((rem, List.nth all_names i) :: o)
  in
  r_helper (List.tl characteristics) []


let split ~characteristics ~examples used_attr =
  let custom_compare (v1, _) (v2, _) =
    if v1 = v2 then 0 else
    if v1 > v2 then 1 else -1
  in
  let helper =
    let rems = get_all_remainders ~examples:examples ~characteristics:characteristics in
    let sorted = List.sort custom_compare rems in
    try Some (List.find (fun (_, attr) -> not (List.mem attr used_attr)) sorted)
    with Not_found -> None
  in
  match helper with
  | None -> exit 0
  | Some x -> x


let get_classification examples positive =
  let get_count_and_size =
    let classes = examples |>
                  List.map (fun e -> if ((List.nth e 0) = positive) then 1 else 0)
    in
    let size = float_of_int (List.length classes) in
    let count = float_of_int (List.fold_left (+) 0 classes) in
    (count, size)
  in

  let majority =
    let (count, size) = get_count_and_size in
    if (count >= size /. 2.) then true else false
  in

  if majority then
    positive
  else
    (*
      Hack to get around the fact that I only track the "positive"
      class instead of both -- this should find the other class
      if there isn't a majority
    *)
    List.filter (fun e -> (List.nth e 0) <> positive) examples |>
    List.map (fun e -> List.nth e 0) |>
    List.hd

let rec split_in_half l1 l2 =
  if (List.compare_lengths l1 l2) > 0 then
  match l1 with 
  | a :: rest -> split_in_half rest (a :: l2)
  | [] -> failwith "Empty list in split_in_half"
  else l1, l2


let split_in4 ex = 
  let e1, e2 = split_in_half ex [] in
  let g1, g2 = split_in_half e1 [] in 
  let g3, g4 = split_in_half e2 [] in
  g1, g2, g3, g4
