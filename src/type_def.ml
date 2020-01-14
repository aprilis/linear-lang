open Util
open Types
open Statics

module StrEnv = Map.Make(String)

let to_vars t1 l = List.map (fun (n, t) -> 
  let n = if l then "!" ^ n else n in
  match t with
    | Some t0 -> (n, TFunc (false, t0, t1))
    | None -> (n, t1)
) >> List.to_seq >> StrEnv.of_seq

let add_lin_type name pt env = { env with
  lintypes = StrEnv.add name pt env.lintypes;
  valid_types = StrEnv.add name true env.valid_types;
  vars = to_vars (TPrim (true, name)) true pt ++ env.vars;
}

let add_type name pt env = { env with
  types = StrEnv.add name pt env.types;
  valid_types = StrEnv.add name false env.valid_types;
  vars = to_vars (TPrim (false, name)) false pt ++ env.vars;
}

let nonlinear_pt = List.map (fun (a, b) -> (a, Option.map nonlinear b))

let add_to_statics (TypeDef (l, n, pt)) =
  if l then add_lin_type n pt << add_type n (nonlinear_pt pt) 
  else add_type n pt