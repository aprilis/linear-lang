val (<<): ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val (>>): ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)

val filter_map: ('a -> 'b option) -> 'a list -> 'b list
val string_of_chars: char list -> string
val seq_to_stream: 'a Seq.t -> 'a Stream.t
val names: string Seq.t
val invert_mapping: ('a, 'b) Hashtbl.t -> ('b, 'a) Hashtbl.t