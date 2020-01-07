open Types
open Util

module StrEnv = Map.Make(String)

let parse_type = Parse.parse_text Parser.type_eof

let math = parse_type "(int, int) -> int"
let comp = parse_type "(int, int) -> bool"
let logic = parse_type "(bool, bool) -> bool"
let cons = parse_type "forall ?a . (a, [a]) -> [a]"
let semicolon = parse_type "forall ?a . ((), a) -> a"

let operator_types_assoc = [
  ([OPlus; OMinus; OMult; ODiv], math);
  ([OGt; OLt; OGeq; OLeq; OEq; ONeq], comp);
  ([OOr; OAnd], logic);
  ([OCons], cons);
  ([OSemicolon], semicolon);
]

let type_defs = List.map (Parse.parse_text Parser.type_def_eof) [
  "type bool = True | False";
]

let other_types = ["int"; "string"; "void"]

let vars = [
  ("fix", "forall ?a . (a -> a) -> a");
  ("len", "forall ?a . [|a|] -> int");
  ("arr_from_elem", "forall a . int -> a -> ![|a|]");
  ("arr_from_list", "forall ?a . [a] -> ![|a|]");
  ("lookup", "forall ?a . int -> [|a|] -> a");
  ("update", "forall ?a . int -> a -> ![|a|] ->  ![|a|]");
  ("drop", "forall ?a . a -> ()");
] |> List.to_seq |> StrEnv.of_seq |> StrEnv.map parse_type

let operator_types op =
  snd @@ List.find (List.mem op << fst) operator_types_assoc

let valid_types = other_types
  |> List.map (fun x -> (x, false)) 
  |> List.to_seq
  |> StrEnv.of_seq

let statics_env = {
  Statics.ops = operator_types;
  Statics.vars = vars;
  Statics.types = StrEnv.empty;
  Statics.lintypes = StrEnv.empty;
  Statics.valid_types = valid_types;
  Statics.type_vars = StrEnv.empty;
} |> List.fold_right Type_def.add_type_def type_defs