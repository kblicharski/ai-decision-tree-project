open Sexplib
open Sexplib.Conv
open Sexplib.Std

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


let sexp_of_dtree (dt: dtree): Sexp.t =
  let get_option = function
  | Some x -> x
  | None -> ""
  in
  let rec helper = function
    | Leaf l ->
      Sexp.(List [
          Atom "LEAF";
          List [
            sexp_of_int l.depth;
            sexp_of_string l.classification;
            sexp_of_string (get_option l.decision)
          ]
        ])
    | Node (s, d) -> Sexp.(List [
        Atom "NODE";
        List [
          sexp_of_int s.depth;
          sexp_of_string s.characteristic;
          sexp_of_string (get_option s.decision);
          sexp_of_string (Printf.sprintf "%.4f" s.remainder);
          List (List.map helper d)
        ]
      ])
  in
  helper dt


let dtree_of_sexp (s: Sexp.t) =
  let rec unpack_list (x: Sexp.t): Sexp.t sexp_list =
    match x with
    | List a -> a
    | _ -> failwith "Not a list"
  and handle_leaf_node (ln: Sexp.t sexp_list) =
    Leaf {
      depth = int_of_sexp (List.nth ln 0);
      classification = string_of_sexp (List.nth ln 1);
      decision = Some (string_of_sexp (List.nth ln 2));
      examples = []
    }
  and handle_split_node (sn: Sexp.t sexp_list) =
    let children = List.map unpack_list (unpack_list (List.nth sn 4)) in
    Node (
      {
        depth = int_of_sexp (List.nth sn 0);
        characteristic = string_of_sexp (List.nth sn 1);
        decision = Some (string_of_sexp (List.nth sn 2));
        remainder = float_of_sexp (List.nth sn 3);
        examples = []
      }, List.map handle_node children
    )
  and handle_node (n: Sexp.t sexp_list) =
    let node_type = List.hd n in
    let node_vals = unpack_list (List.nth n 1) in
    match node_type with
    | Atom "NODE" -> handle_split_node node_vals
    | Atom "LEAF" -> handle_leaf_node node_vals
    | Atom e -> failwith (Printf.sprintf "Unknown node type: %s" e)
    | _ -> failwith "Did not expect `node_type` to be a list"
  in
  handle_node (unpack_list s)