open Model
open Sexplib

let find x l =
  let rec find_helper x l n =
    match l with
    | [] -> failwith "Item not found in list"
    | h :: r -> if h = x then n else find_helper x r (n+1)
  in
  find_helper x l 0


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
    let a = if p > 0. then -.(p /. z) *. (log2 (p /. z)) else 0. in
    let b = if n > 0. then -.(n /. z) *. (log2 (n /. z)) else 0. in
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


let get_all_remainders ~model ~examples =
  let rec r_helper chars o =
    match chars with
    | [] -> o
    | h :: r ->
      let i = find h model.characteristics in
      let p_and_d = partition examples model.decisions i in
      let p = List.map (fun (_, p) -> p) p_and_d in
      let rem = remainder examples p model.positive in
      r_helper r ((rem, List.nth model.characteristics i) :: o)
  in
  r_helper (List.tl model.characteristics) []


let split ~model ~examples used_attr =
  let custom_compare (v1, _) (v2, _) =
    if v1 = v2 then 0 else
    if v1 > v2 then 1 else -1
  in
  let helper =
    let rems = get_all_remainders ~examples: examples ~model: model in
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


let print_source ?(channel = stdout) sexp =
  let formatter = Format.formatter_of_out_channel channel in
  Sexp.pp_hum formatter sexp;
  Printf.printf "\n\n";
  Format.pp_print_flush formatter ()


let print_tree dt =
  let sexp = sexp_of_dtree dt in
  print_source sexp ;