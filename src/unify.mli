type var_t = int * string 
exception Error of string * var_t Types.typ * var_t Types.typ
val unify: string Types.typ -> string Types.typ -> string Types.typ
val unify_and_infer: string Types.typ list -> string Types.typ list
  -> string Types.typ -> string Types.typ