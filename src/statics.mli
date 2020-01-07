open Util
open Types

exception RepeatedVariable of string * pattern
exception TypeError of string * expr

type op_env = operator -> string typ
type env = {
  ops: op_env;
  vars: string typ str_env;
  types: prim_type str_env;
  lintypes: prim_type str_env;
  valid_types: linearity str_env;
  type_vars: string str_env;
}

val infer_type: env -> expr -> string typ