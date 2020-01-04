open Types
open Util

let triple x = (TPrim x, TPrim x, TPrim x)
let comp = (TPrim "int", TPrim "int", TPrim "bool")
let nonlin (a, b, c) = ((false, a), (false, b), (false, c))

let operator_types = nonlin << function
  | OPlus   -> triple "int"
  | OMinus  -> triple "int"
  | OMult   -> triple "int"
  | ODiv    -> triple "int"
  | OGt     -> comp
  | OLt     -> comp
  | OGeq    -> comp
  | OLeq    -> comp
  | OEq     -> comp
  | ONeq    -> comp
  | OOr     -> triple "bool"
  | OAnd    -> triple "bool"
  | OCons   -> (TVar "a", TList (false, TVar "a"), TList (false, TVar "a"))