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

type 'a pattern = 
  | PVar of 'a 
  | PWild 
  | PTuple of 'a pattern list 
  | PCons of 'a pattern * 'a pattern
  | PEmptyList
  | PConstr of string * 'a pattern option

type operator = OPlus | OMinus | OMult | OAnd | OOr | OCons

type 'a expr = 
  | EFun of linearity * 'a pattern * string typ * 'a expr 
  | EROLet of 'a list * 'a pattern * 'a expr * 'a expr 
  | ELet of 'a pattern * 'a expr * 'a expr 
  | ECase of 'a expr * ('a pattern * 'a expr) list 
  | EIf of 'a expr * 'a expr * 'a expr
  | EOp of operator * 'a expr * 'a expr
  | EApp of 'a expr * 'a expr 
  | ETuple of 'a expr list 
  | EEmptyList 
  | EArray of 'a expr list 
  | EInt of int 
  | EString of string 
  | EVar of 'a

type prog = type_def list * string expr

let rec map f (l, t) =
  let t = match t with
    | TPrim s -> TPrim s
    | TVar x -> TVar x
    | TNonLinVar x -> TNonLinVar x
    | TFunc (a, b) -> TFunc (map f a, map f b)
    | TTuple l -> TTuple (List.map (map f) l)
    | TList l -> TList (map f l)
    | TArray l -> TArray (map f l)
  in (l, f t)

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