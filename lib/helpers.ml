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