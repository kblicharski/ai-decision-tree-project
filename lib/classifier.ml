open Model

let classify_all ~model ~examples ~tree =
  let rec find_correct_branch (choice: string) (branches: dtree list) = 
    let check_branch ch = function
      | Leaf n -> if ch = (Helpers.get_option n.decision) then true else false
      | Node (n, _) -> if ch = (Helpers.get_option n.decision) then true else false
    in
    match branches with
    | h :: r -> 
      let valid = check_branch choice h in
      if valid then h else find_correct_branch choice r
    | [] -> failwith "Error: No valid branch found"
  in
  let rec check ex = function
    | Leaf n      ->
      if n.classification = List.nth ex 0 then true else false
    | Node (n, l) ->
      let index = Helpers.find n.characteristic model.characteristics in
      let choice = List.nth ex index in
      let new_tree = find_correct_branch choice l in
      check ex new_tree
  in
  let rec classify ic = function
    | h :: r -> 
      if check h tree then classify ic r else classify (ic + 1) r
    | [] -> ic
  in
  classify 0 examples