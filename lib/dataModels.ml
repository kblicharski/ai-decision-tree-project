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

let decisions5 = ["x";"o";"b"]
let characteristics5 = [
  ("class", ["win";"loss";"draw"]);
  ("a1", decisions5);
  ("a2", decisions5);
  ("a3", decisions5);
  ("a4", decisions5);
  ("a5", decisions5);
  ("a6", decisions5);
  ("b1", decisions5);
  ("b2", decisions5);
  ("b3", decisions5);
  ("b4", decisions5);
  ("b5", decisions5);
  ("b6", decisions5);
  ("c1", decisions5);
  ("c2", decisions5);
  ("c3", decisions5);
  ("c4", decisions5);
  ("c5", decisions5);
  ("c6", decisions5);
  ("d1", decisions5);
  ("d2", decisions5);
  ("d3", decisions5);
  ("d4", decisions5);
  ("d5", decisions5);
  ("d6", decisions5);
  ("e1", decisions5);
  ("e2", decisions5);
  ("e3", decisions5);
  ("e4", decisions5);
  ("e5", decisions5);
  ("e6", decisions5);
  ("f1", decisions5);
  ("f2", decisions5);
  ("f3", decisions5);
  ("f4", decisions5);
  ("f5", decisions5);
  ("f6", decisions5);
  ("g1", decisions5);
  ("g2", decisions5);
  ("g3", decisions5);
  ("g4", decisions5);
  ("g5", decisions5);
  ("g6", decisions5);
]

let characteristics_for file =
  match file with
  | "house-votes-84" | "votes-small" -> characteristics1
  | "car" -> characteristics2
  | "tic-tac-toe" -> characteristics3
  | "balance-scale" -> characteristics4
  | "connect-4" -> characteristics5
  | _ -> failwith "data model not found"
