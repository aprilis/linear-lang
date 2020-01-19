open Util
open Types

type value = 
  | VInt of int 
  | VChar of char 
  | VCons of lazy_value * lazy_value
  | VEmptyList
  | VTuple of lazy_value list
  | VArray of lazy_value array
  | VConstr of string * lazy_value option
  | VFunc of (lazy_value -> lazy_value)
and lazy_value = value Lazy.t

type op_env = operator -> lazy_value -> lazy_value -> value
type env = {
  ops: op_env;
  vars: value str_env;
}

exception RuntimeError of string

val eval: env -> expr -> lazy_value
val to_list: lazy_value -> lazy_value list