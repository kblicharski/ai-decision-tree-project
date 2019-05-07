let find x l =
  let rec find_helper x l n =
    match l with
    | [] -> failwith "Item not found in list"
    | h :: r -> if h = x then n else find_helper x r (n+1)
  in
  find_helper x l 0

(*
  
*)
let uniform_partition ~partition =
  let p = 
    partition |> 
    List.map (fun e -> List.length e) |> 
    List.filter (fun l -> l <> 0) in
  if List.length p = 1 then Some p else None

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
      partition_uniq r (matched :: o)
  in
  List.rev (partition_uniq u [])

(* Simple function to calculate log base 2 *)
let log2 v = Pervasives.log10(v) /. Pervasives.log10(2.)

let entropy p n = 
  let z = p +. n in
  let a = if p > 0. then -.(p /. z) *. (log2 (p /. z)) else 0. in
  let b = if n > 0. then -.(n /. z) *. (log2 (n /. z)) else 0. in
  a +. b 

let remainder examples partitioned pos =
  let find_p_n ex = 
    let (p_, n_) = List.partition (fun e -> (List.nth e 0) = pos) ex in
    (float_of_int (List.length p_), float_of_int (List.length n_))
  in
  let (p, n) = find_p_n examples in
  (* let () = Printf.printf "p:%f n:%f\n" p n in *)
  let pis_nis = List.map find_p_n partitioned in
  let calc_remainder (pi, ni) =
    let z = pi +. ni in
    (* let () = Printf.printf "pi:%f ni:%f\n" pi ni in *)
    let rem = (z /. (p +. n)) *. (entropy (pi /. z) (ni /. z)) in
    (* let () = Printf.printf "Entropy:%f\n" (entropy (pi /. z) (ni /. z)) in *)
    (* let () = Printf.printf "Remainder:%f\n" rem in *)
    rem
  in
  let remainders = List.map calc_remainder pis_nis in
  (* let () = List.iter (Printf.printf "%f ") remainders in *)
  let sum l = List.fold_right (+.) l 0. in
  sum remainders