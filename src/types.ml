open Util

type linearity = bool
type var_linearity = Lin | NonLin | AnyLin

type 'a typ = 
  | TPrim of linearity * string 
  | TVar of var_linearity * 'a 
  | TFunc of linearity * 'a typ * 'a typ 
  | TTuple of 'a typ list        
  | TList of 'a typ 
  | TArray of linearity * 'a typ

type prim_type = (string * string typ option) list
type type_def = TypeDef of linearity * string * prim_type

type pattern = 
  | PVar of string
  | PWild 
  | PTuple of pattern list 
  | PCons of pattern * pattern
  | PEmptyList
  | PConstr of linearity * string * pattern option

type operator = OPlus | OMinus | OMult | ODiv
              | OGt | OLt | OGeq | OLeq | OEq | ONeq 
              | OAnd | OOr | OCons | OConcat | OSemicolon | OPipe

type expr = 
  | EFun of linearity * (string * var_linearity) list * pattern * string typ * expr 
  | EROLet of string list * pattern * expr * expr 
  | ELet of pattern * expr * expr 
  | ECase of expr * (pattern * expr) list 
  | EIf of expr * expr * expr
  | EOp of operator * expr * expr
  | EApp of expr * expr 
  | ETuple of expr list 
  | EEmptyList 
  | EArray of expr list 
  | EInt of int 
  | EChar of char 
  | EVar of string

type prog = type_def list * string list * expr

let rec is_linear = function
  | TPrim (l, _)
  | TFunc (l, _, _) -> l
  | TVar (l, _) -> l == Lin
  | TTuple l -> List.exists is_linear l
  | TList l -> is_linear l
  | TArray (l, _) -> l

let accepts_linear l = l <> NonLin

let rec map f t =
  match t with
    | TPrim (_, _)
    | TVar (_, _) -> f t
    | TFunc (l, a, b) -> f @@ TFunc (l, map f a, map f b)
    | TTuple l -> f @@ TTuple (List.map (map f) l)
    | TList l -> f @@ TList (map f l)
    | TArray (l, a) -> f @@ TArray (l, map f a)

let rec map_var f t =
  match t with
    | TPrim (l, s) -> TPrim (l, s)
    | TVar (l, x) -> TVar (l, f x)
    | TFunc (l, a, b) -> TFunc (l, map_var f a, map_var f b)
    | TTuple l -> TTuple (List.map (map_var f) l)
    | TList l -> TList (map_var f l)
    | TArray (l, a) -> TArray (l, map_var f a)

let rec fold f t a =
  let a = f t a in
  match t with
    | TFunc (_, x, y) -> a |> fold f x |> fold f y
    | TTuple l -> List.fold_right (fold f) l a
    | TList l
    | TArray (_, l) -> fold f l a
    | _ -> a

let nonlinear t = map (function
  | TPrim (true, s) -> TPrim (false, s)
  | TFunc (true, _, _) -> TPrim (false, "void")
  | TArray (true, t) -> TArray (false, t)
  | t -> t
) t

let type_var_list t =
  let rec unique_sorted = function
    | (h1, _)::(h2, x)::t when h1 = h2 -> unique_sorted ((h2, x)::t)
    | h::t -> h :: unique_sorted t
    | [] -> []
    in
  fold List.cons t []
  |> Util.filter_map (function 
    | TVar (l, x) -> Some (x, l)
    | _ -> None)
  |> List.sort compare |> unique_sorted

let unroll_list items =
  List.fold_right (fun x y -> EOp(OCons, x, y)) items EEmptyList