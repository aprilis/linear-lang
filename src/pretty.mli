val print_type: Format.formatter -> string Types.typ -> unit
val print_expr: Format.formatter -> Types.expr -> unit
val print_pattern: Format.formatter -> Types.pattern -> unit
val print_list: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit