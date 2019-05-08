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
      model: data_model;
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
      model: data_model;
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
  let rec helper dt' =
    match dt' with
    | Leaf l -> 
      Sexp.(List [
        Atom "LEAF";
        sexp_of_int l.depth;
        sexp_of_string l.classification;
        sexp_of_option (fun a -> sexp_of_string a) l.decision;
        (* sexp_of_list (fun e -> sexp_of_list (fun e2 -> sexp_of_string e2) e) l.examples *)
      ])
    | Node (s, d) -> Sexp.(List [
        Atom "NODE";
        sexp_of_int s.depth;
        sexp_of_string s.characteristic;
        sexp_of_option (fun a -> sexp_of_string a) s.decision;
        sexp_of_float s.remainder;
        (* sexp_of_list (fun e -> sexp_of_list (fun e2 -> sexp_of_string e2) e) s.examples; *)
        sexp_of_list (fun e -> helper e) d
    ])
  in
  helper dt