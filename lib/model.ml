type data_model = {
  positive: string;
  characteristics: string list;
  decisions: string list;
}

module SNode = struct
  type t = 
    {
      model: data_model;
      depth: int;
      characteristic: string;
      decision: string;
      remainder: float;
      examples: string list list;
    }
end

module LNode = struct
  type t = 
    {
      model: data_model;
      depth: int;
      classification: string;
      decision: string;
      examples: string list list;
    }
end

type dtree = 
  | Leaf of LNode.t
  | Node of SNode.t * dtree list

