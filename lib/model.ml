open Sexplib
open Sexplib.Conv

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

let sexp_of_dtree (dt: dtree) =
  let get_option = function
  | Some x -> x
  | None -> ""
  in
  let rec helper dt' =
    match dt' with
    | Leaf l ->
      Sexp.(List [
          Atom "LEAF";
          sexp_of_int l.depth;
          sexp_of_string l.classification;
          sexp_of_string (get_option l.decision)
        ])
    | Node (s, d) -> Sexp.(List [
        Atom "NODE";
        sexp_of_int s.depth;
        sexp_of_string s.characteristic;
        sexp_of_string (get_option s.decision);
        sexp_of_string (Printf.sprintf "%.4f" s.remainder);
        List [
          sexp_of_list (fun e -> helper e) d
        ]
      ])
  in
  helper dt
