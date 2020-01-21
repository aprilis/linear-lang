open Types
open Util

module StrEnv = Map.Make(String)

let parse_type = Parse.parse_text Parser.type_eof

let math = parse_type "(int, int) -> int"
let comp = parse_type "(int, int) -> bool"
let logic = parse_type "(bool, bool) -> bool"
let cons = parse_type "<?a> (!a, [!a]) -> [!a]"
let concat = parse_type "<?a> ([!a], [!a]) -> [!a]"
let semicolon = parse_type "<?a> ((), !a) -> !a"
let pipe = parse_type "<?a, ?b> (!a, !a -> !b) -> !b"

let operator_types_assoc = [
  ([OPlus; OMinus; OMult; ODiv], math);
  ([OGt; OLt; OGeq; OLeq; OEq; ONeq], comp);
  ([OOr; OAnd], logic);
  ([OCons], cons);
  ([OConcat], concat);
  ([OSemicolon], semicolon);
  ([OPipe], pipe)
]

let type_defs = List.map (Parse.parse_text Parser.type_def_opt_eof) [
  "type bool = True | False";
  "type char";
  "type int";
  "type void";
  "type line_opt = Line [char] | EOF";
  "type !stdin";
  "type !stdout";
]

let vars = [
  ("fix", "<?a> (!a -> !a) -> !a");
  ("len", "<?a> [|!a|] -> int");
  ("arr_from_elem", "<a> int -> a -> ![|a|]");
  ("arr_from_list", "<?a> [!a] -> ![|!a|]");
  ("lookup", "<a> int -> [|a|] -> a");
  ("update", "<?a> (!a -> !a) -> int -> ![|!a|] -> ![|!a|]");
  ("drop", "<?a> ![|!a|] -> ()");
  ("print", "[char] -> !stdout -> !stdout");
  ("read_line", "!stdin -> (line_opt, !stdin)");
  ("int_of_string", "[char] -> int");
  ("string_of_int", "int -> [char]")
] |> List.to_seq |> StrEnv.of_seq |> StrEnv.map parse_type

let used_vars = [
  ("stdin", "!stdin");
  ("stdout", "!stdout");
] |> List.to_seq |> StrEnv.of_seq |> StrEnv.map parse_type

let operator_types op =
  snd @@ List.find (List.mem op << fst) operator_types_assoc

let valid_types = StrEnv.empty

let statics_env_default = {
  Statics.ops = operator_types;
  Statics.vars = vars;
  Statics.types = StrEnv.empty;
  Statics.lintypes = StrEnv.empty;
  Statics.valid_types = valid_types;
  Statics.type_vars = StrEnv.empty;
} |> List.fold_right Type_def.add_to_statics type_defs

let statics_env used = 
  let u = StrEnv.filter (fun x _ -> List.mem x used) used_vars in
  { statics_env_default with vars = statics_env_default.vars ++ u }

open Eval

let vbool x = VConstr ((if x then "True" else "False"), None)
let from_vbool (lazy (VConstr(x, None))) = x = "True"

let opM f (lazy (VInt a)) (lazy (VInt b)) =
  VInt (f a b)
let opC f (lazy (VInt a)) (lazy (VInt b)) =
  vbool (f a b)

let rec concat (lazy a) b =
  match a with
  | VEmptyList -> Lazy.force b
  | VCons (h, t) -> VCons (h, lazy (concat t b))

let operator_impl = function
  | OPlus -> opM (+)
  | OMinus -> opM (-)
  | OMult -> opM ( * )
  | ODiv -> fun (lazy (VInt a)) (lazy (VInt b)) ->
      if b == 0 then raise @@ RuntimeError "Division by zero" else VInt (a / b)
  | OGt -> opC (>)
  | OGeq -> opC (>=)
  | OLt -> opC (<)
  | OLeq -> opC (<=)
  | OEq -> opC (=)
  | ONeq -> opC (<>)
  | OAnd -> fun a b -> vbool (from_vbool a && from_vbool b)
  | OOr -> fun a b -> vbool (from_vbool a || from_vbool b)
  | OCons -> fun h t -> VCons (h, t)
  | OConcat -> concat
  | OSemicolon -> fun (lazy _) (lazy x) -> x
  | OPipe -> fun a (lazy (VFunc b)) -> Lazy.force @@ b a

let vfunc f = VFunc (fun x -> lazy (f @@ Lazy.force x))
let vfunc2 f = vfunc (vfunc << f)

let unit_const = VTuple []

let string_to_vchar_list =
  String.to_seq >> List.of_seq >>
  List.map (fun c -> Lazy.from_val (VChar c)) >>
  flip (List.fold_right (fun x y -> Lazy.from_val (VCons (x, y))))
    (Lazy.from_val VEmptyList)
  
let vchar_list_to_string =
  to_list >>
  List.map (fun (lazy (VChar c)) -> c) >>
  string_of_chars

let vars = [
  ("fix", let rec fix f = lazy (
      let lazy (VFunc g) = f in 
      let x = lazy (Lazy.force @@ fix f) in
      Lazy.force @@ g x)
    in VFunc fix);
  ("len", vfunc (fun (VArray a) -> VInt (Array.length a)));
  ("arr_from_elem", vfunc (fun (VInt s) -> 
                    VFunc (fun x -> 
                      lazy (VArray (Array.make s x)))));
  ("arr_from_list", VFunc (fun x -> lazy (VArray (to_list x |> Array.of_list))));
  ("lookup", vfunc2 (fun (VInt i) (VArray a) ->
    try Lazy.force a.(i) with Invalid_argument _ -> raise @@ RuntimeError "Invalid array index"));
  ("update", vfunc2 (fun (VFunc f) (VInt i) ->
    VFunc (fun (lazy (VArray a)) -> try lazy(a.(i) <- f (a.(i)); VArray a) with
      Invalid_argument _ -> raise @@ RuntimeError "Invalid array index")));
  ("drop", vfunc (fun _ -> unit_const));
  ("print", vfunc2 (fun v s -> Lazy.from_val v |> vchar_list_to_string |> print_string;
                               s));
  ("read_line", vfunc (fun s ->
    let res =
      try let line = read_line () |> string_to_vchar_list in
          VConstr ("Line", Some line)
      with End_of_file -> VConstr ("EOF", None)
      in
    VTuple [Lazy.from_val res; Lazy.from_val s]));
  ("int_of_string", vfunc (fun v -> 
    try VInt (Lazy.from_val v |> vchar_list_to_string |> int_of_string)
    with Failure  _ -> raise @@ RuntimeError "int_of_string failure"));
  ("string_of_int", vfunc (fun (VInt x) -> string_of_int x |>
                                           string_to_vchar_list |>
                                           Lazy.force))
] |> List.to_seq |> StrEnv.of_seq

let used_vars = [
  ("stdin", unit_const);
  ("stdout", unit_const)
] |> List.to_seq |> StrEnv.of_seq

let runtime_env_default = {
  ops = operator_impl;
  vars = vars;
} |> List.fold_right Type_def.add_to_runtime type_defs

let runtime_env used = 
  let u = StrEnv.filter (fun x _ -> List.mem x used) used_vars in
  { runtime_env_default with vars = runtime_env_default.vars ++ u }