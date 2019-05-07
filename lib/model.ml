type data_model = {
  positive: string;
  characteristics: string list;
  decisions: string list;
}

type dtree =
  | Leaf of node
  | Node of node * dtree list
and node = {
  model: data_model;
  depth: int;
  parent: node option;
  characteristic: string option;
  decision: string;
  remainder: float;
  examples: string list list;
}
