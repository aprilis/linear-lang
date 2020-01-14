type 'a str_env = 'a Map.Make(String).t

val (=>): bool -> bool -> bool
val (<<): ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val (>>): ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)

val filter_map: ('a -> 'b option) -> 'a list -> 'b list
val fold_map: ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val string_of_chars: char list -> string
val seq_to_stream: 'a Seq.t -> 'a Stream.t
val names: string Seq.t
val (++): 'a str_env -> 'a str_env -> 'a str_env
val (--): 'a str_env -> 'a str_env -> 'a str_env

val const: 'a -> 'b -> 'a
val negate: ('a -> bool) -> 'a -> bool
val id: 'a -> 'a

val flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c