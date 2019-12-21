type id = string
type linearity = bool

type styp = TPrim of id | TFunc of typ * typ | TTuple of typ list 
          | TList of typ | TArray of typ
and typ = linearity * styp

type type_def = TypeDef of linearity * id * (id * typ option) list

type pattern = PVar of id | PWild | PTuple of pattern list | PCons of pattern * pattern
             | PList of pattern list | PConstr of id * pattern

type operator = OPlus | OMinus | OMult | OAnd | OOr | OCons

type expr = EFun of linearity * pattern * typ * expr | EROLet of pattern * id list * expr * expr |
            ELet of pattern * expr * expr | ECase of expr * (pattern * expr) list |
            EIf of expr * expr * expr | EOp of operator * expr * expr |
            EApp of expr * expr | ETuple of expr list | EList of expr list |
            EArray of expr list | EInt of int | EString of string | EVar of id

type prog = type_def list * expr