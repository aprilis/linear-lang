open Types
open Util

module StrEnv = Map.Make(String)

let parse_type = Parse.parse_text Parser.type_eof

let math = parse_type "(int, int) -> int"
let comp = parse_type "(int, int) -> bool"
let logic = parse_type "(bool, bool) -> bool"
let cons = parse_type "<?a> (!a, [!a]) -> [!a]"
let semicolon = parse_type "<?a> ((), !a) -> !a"

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
  ("fix", "<?a> (!a -> !a) -> !a");
  ("len", "<?a> [|!a|] -> int");
  ("arr_from_elem", "<a> int -> a -> ![|a|]");
  ("arr_from_list", "<?a> [!a] -> ![|!a|]");
  ("lookup", "<a> [|a|] -> int -> a");
  ("update", "<?a> ![|!a|] -> int -> !a -> ![|!a|]");
  ("drop", "<!a> !a -> ()");
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
} |> List.fold_right Type_def.add_to_statics type_defs


open Eval

let vbool x = VConstr ((if x then "True" else "False"), None)
let from_vbool (lazy (VConstr(x, None))) = x = "True"

let opM f (lazy (VInt a)) (lazy (VInt b)) =
  VInt (f a b)
let opC f (lazy (VInt a)) (lazy (VInt b)) =
  vbool (f a b)

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
  | OAnd -> (fun a b -> vbool (from_vbool a && from_vbool b))
  | OOr -> (fun a b -> vbool (from_vbool a || from_vbool b))
  | OCons -> fun h t -> VCons (h, t)
  | OSemicolon -> fun (lazy _) (lazy x) -> x

let vfunc f = VFunc (fun x -> lazy (f @@ Lazy.force x))
let vfunc2 f = vfunc (vfunc << f)

let vars = [
  ("fix", let rec fix f = lazy (
      let lazy (VFunc g) = f in 
      let x = lazy (Lazy.force @@ fix f) in
      Lazy.force @@ g x)
    in VFunc fix);
  ("len", vfunc (fun (VArray a) -> VInt (Array.length a)));
  ("arr_from_elem", vfunc2 (fun (VInt s) x -> VArray (Array.make s (Lazy.from_val x))));
  ("arr_from_list", VFunc (fun x -> lazy (VArray (to_list x |> Array.of_list))));
  ("lookup", vfunc2 (fun (VArray a) (VInt i) ->
    try Lazy.force a.(i) with Invalid_argument _ -> raise @@ RuntimeError "Invalid array index"));
  ("update", vfunc2 (fun (VArray a) (VInt i) ->
    VFunc (fun x -> try lazy(a.(i) <- x; VArray a) with
      Invalid_argument _ -> raise @@ RuntimeError "Invalid array index")));
  ("drop", vfunc (fun _ -> VTuple []))
] |> List.to_seq |> StrEnv.of_seq

let runtime_env = {
  ops = operator_impl;
  vars = vars;
}