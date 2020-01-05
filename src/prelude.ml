open Types
open Util

module StrEnv = Map.Make(String)

let parse_type = Parse.parse_text Parser.type_eof

let math = parse_type "(int, int) -> int"
let comp = parse_type "(int, int) -> bool"
let logic = parse_type "(bool, bool) -> bool"
let cons = parse_type "forall ?a . (a, [a]) -> [a]"

let operator_types_assoc = [
  ([OPlus; OMinus; OMult; ODiv], math);
  ([OGt; OLt; OGeq; OLeq; OEq; ONeq], comp);
  ([OOr; OAnd], logic);
  ([OCons], cons)
]

let operator_types op =
  snd @@ List.find (List.mem op << fst) operator_types_assoc

let type_defs = List.map (Parse.parse_text Parser.type_def_eof) [
  "type bool = True | False";
]

let other_types = ["int"; "string"; "void"]

let valid_types = other_types
  |> List.map (fun x -> (x, false)) 
  |> List.to_seq
  |> StrEnv.of_seq

let statics_env = {
  Statics.ops = operator_types;
  Statics.vars = StrEnv.empty;
  Statics.types = StrEnv.empty;
  Statics.lintypes = StrEnv.empty;
  Statics.valid_types = valid_types;
  Statics.type_vars = StrEnv.empty;
} |> List.fold_right Type_def.add_type_def type_defs