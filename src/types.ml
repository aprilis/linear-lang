type linearity = bool

type 'a styp = 
  | TPrim of string 
  | TVar of 'a 
  | TNonLinVar of 'a 
  | TFunc of 'a typ * 'a typ 
  | TTuple of 'a typ list        
  | TList of 'a typ 
  | TArray of 'a typ
and 'a typ = linearity * 'a styp

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
              | OAnd | OOr | OCons

type expr = 
  | EFun of linearity * pattern * string typ * expr 
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
  | EString of string 
  | EVar of string

type prog = type_def list * expr

let linear = function
  | TPrim _
  | TVar _
  | TNonLinVar _
  | TFunc _ -> false
  | TTuple l -> List.exists fst l
  | TList (l, _) -> l
  | TArray (l, _) -> l

let rec map f (l, t) =
  let t = match t with
    | TPrim s -> TPrim s
    | TVar x -> TVar x
    | TNonLinVar x -> TNonLinVar x
    | TFunc (a, b) -> TFunc (map f a, map f b)
    | TTuple l -> TTuple (List.map (map f) l)
    | TList l -> TList (map f l)
    | TArray l -> TArray (map f l)
  in f (l, t)

let rec map_var f (l, t) =
  (l, match t with
    | TPrim s -> TPrim s
    | TVar x -> TVar (f x)
    | TNonLinVar x -> TNonLinVar (f x)
    | TFunc (a, b) -> TFunc (map_var f a, map_var f b)
    | TTuple l -> TTuple (List.map (map_var f) l)
    | TList l -> TList (map_var f l)
    | TArray l -> TArray (map_var f l)
  )

let rec fold f (l, t) a =
  let a = f (l, t) a in
  match t with
    | TFunc (x, y) -> a |> fold f x |> fold f y
    | TTuple l -> List.fold_right (fold f) l a
    | TList l
    | TArray l -> fold f l a
    | _ -> a