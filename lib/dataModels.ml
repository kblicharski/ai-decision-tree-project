let decisions1 = ["y"; "n"; "?"]
let positive1 = "democrat"
let characteristics1 = [
  ("party", decisions1);
  ("handicapped-infants", decisions1);
  ("water-project-cost-sharing", decisions1);
  ("adoption-of-the-budget-resolution", decisions1);
  ("physician-fee-freeze", decisions1);
  ("el-salvador-aid", decisions1);
  ("religious-groups-in-schools", decisions1);
  ("anti-satellite-test-ban", decisions1);
  ("aid-to-nicaraguan-contras", decisions1);
  ("mx-missile", decisions1);
  ("immigration", decisions1);
  ("synfuels-corporation-cutback", decisions1);
  ("education-spending", decisions1);
  ("superfund-right-to-sue", decisions1);
  ("crime", decisions1);
  ("duty-free-exports", decisions1);
  ("export-administration-act-south-africa", decisions1);
]

let characteristics2 = [
  ("car_quality", ["unacc"; "acc"; "good"; "vgood"]);
  ("buying", ["vhigh"; "high"; "med"; "low"]);
  ("maint", ["vhigh"; "high"; "med"; "low"]);
  ("doors", ["2"; "3"; "4"; "5more"]);
  ("persons", ["2"; "4"; "more"]);
  ("lug_boot", ["small"; "med"; "big"]);
  ("safety", ["low"; "med"; "high"]);
]

let decisions3 = ["x";"o";"b"]
let characteristics3 = [
  ("class", ["positive"; "negative"]);
  ("top-left-square", decisions3);
  ("top-middle-square", decisions3);
  ("top-right-square", decisions3);
  ("middle-left-square", decisions3);
  ("middle-middle-square", decisions3);
  ("middle-right-square", decisions3);
  ("bottom-left-square", decisions3);
  ("bottom-middle-square", decisions3);
  ("bottom-right-square", decisions3);
]

let decisions4 = ["1";"2";"3";"4";"5"]
let characteristics4 = [
  ("class", ["L"; "B"; "R"]);
  ("Left-Weight", decisions4);
  ("Left-Distance", decisions4);
  ("Right-Weight", decisions4);
  ("Right-Distance", decisions4);
]

let characteristics_for file =
  match file with
  | "house-votes-84" | "votes-small" -> characteristics1
  | "car" -> characteristics2
  | "tic-tac-toe" -> characteristics3
  | "balance-scale" -> characteristics4
  | _ -> failwith "data model not found"
