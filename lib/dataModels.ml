let characteristics1 = [
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


let characteristics_for file =
  match file with
  | "house-votes-84" | "votes-small" -> characteristics1
  | _ -> failwith "data model not found"

let decisions_for file =
  match file with
  | "house-votes-84" | "votes-small" -> decisions1
  | _ -> failwith "data model not found"

let positive_for file =
  match file with
  | "house-votes-84" | "votes-small" -> positive1
  | _ -> failwith "data model not found"
