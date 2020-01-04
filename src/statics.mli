exception RepeatedVariable of string * Types.pattern
exception UnboundVariable of string
exception TypeError of string * Types.expr

type op_env = Types.operator -> (string Types.typ * string Types.typ * string Types.typ)
type env = {
  ops: op_env;
  vars: string Types.typ Map.Make(String).t;
  types: Types.prim_type Map.Make(String).t;
  lintypes: Types.prim_type Map.Make(String).t;
}

val infer_type: env -> Types.expr -> string Types.typ