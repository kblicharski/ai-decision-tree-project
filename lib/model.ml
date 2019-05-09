type data_model = {
  positive: string;
  characteristics: string list;
  decisions: string list;
}

module SNode = struct
  type t =
    {
      depth: int;
      characteristic: string;
      decision: string option;
      remainder: float;
      examples: string list list;
    }
end

module LNode = struct
  type t =
    {
      depth: int;
      classification: string;
      decision: string option;
      examples: string list list;
    }
end

type dtree =
  | Leaf of LNode.t
  | Node of SNode.t * dtree list [@@deriving sexp]