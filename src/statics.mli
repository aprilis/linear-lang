exception RepeatedVariable of string * string Types.pattern
exception UnboundVariable of string
exception TypeError of string * int Types.expr
val to_int_ids: string Types.expr -> int Types.expr