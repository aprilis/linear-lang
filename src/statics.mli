exception RepeatedVariable of string * string Types.pattern
exception UnboundVariable of string
exception TypeError of string * int Types.expr

type op_env = Types.operator -> (string Types.typ * string Types.typ * string Types.typ)
type typ_env = unit

val to_int_ids: string Types.expr -> int Types.expr
val infer_type: typ_env -> typ_env -> op_env -> int Types.expr -> string Types.typ