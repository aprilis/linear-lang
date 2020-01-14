open Types
open Util
open Format

let type_var_str (x, l) =
  let prefix = match l with
    | Lin -> "!"
    | NonLin -> ""
    | AnyLin -> "?"
  in prefix ^ x

let print_str ppf s = fprintf ppf "%s" s
let print_int ppf s = fprintf ppf "%d" s

let lpar ppf yes = if yes then print_str ppf "("
let rpar ppf yes = if yes then print_str ppf ")"

let print_list f ppf l =
  match l with
    | h::t ->
      f ppf h;
      List.iter (fprintf ppf ",@ %a" f) t
    | [] -> ()

let print_lin ppf l = if l then print_str ppf "!"
let print_arrow ppf l = print_str ppf (if l then "-o" else "->")

let print_typ ppf t = 
  open_hovbox 1;
  let vars = type_var_list t |> List.map type_var_str in
  if vars <> [] then fprintf ppf "<%a>@ " (print_list print_str) vars;
  let rec aux rarrow ppf t =
    open_hovbox 1;
    begin match t with
      | TArray (l, s) -> fprintf ppf "%a[|%a|]" print_lin l (aux false) s
      | TFunc (l, x, y) -> 
          lpar ppf rarrow;
          fprintf ppf "%a@ %a@ %a" (aux true) x print_arrow l (aux false) y;
          rpar ppf rarrow
      | TList s -> fprintf ppf "[%a]" (aux false) s
      | TPrim (l, s) -> fprintf ppf "%a%s" print_lin l s
      | TVar (_, s) -> print_str ppf s
      | TTuple l -> fprintf ppf "(%a)" (print_list (aux false)) l
    end;
    close_box ()
  in aux false ppf t;
  close_box ()

let arrow_str lin = if lin then "-o" else "->"
let op_str op = 
  match op with
    | OAnd -> "&&"
    | OCons -> "::"
    | OMinus -> "-"
    | OMult -> "*"
    | ODiv -> "/"
    | OGt -> ">"
    | OLt -> "<"
    | OGeq -> ">="
    | OLeq -> "<="
    | OEq -> "=="
    | ONeq -> "!="
    | OOr -> "||"
    | OPlus -> "+"
    | OSemicolon -> ";"

let rec print_pat ppf p =
  match p with
    | PVar x -> print_str ppf x
    | PWild -> print_str ppf "_"
    | PTuple l -> fprintf ppf "(%a)" (print_list print_pat) l
    | PCons(h, t) -> fprintf ppf "%a::%a" print_pat h print_pat t
    | PEmptyList -> print_str ppf "[]"
    | PConstr(l, c, None) -> fprintf ppf "%a%s" print_lin l c
    | PConstr(l, c, Some p) -> fprintf ppf "%a%s %a" print_lin l c print_pat p

let print_type_vars ppf tv =
  if tv <> [] then
    Format.fprintf ppf "<%a> " (print_list print_str) (List.map type_var_str tv)

let print_exp = 
  let rec print_e ppf e =
    open_hovbox 1;
    begin match e with
      | EFun(lin, tv, pat, typ, e) ->
          fprintf ppf "fun %a(%a :@ %a) %s@ %a" print_type_vars tv print_pat pat print_typ typ (arrow_str lin) print_e e
      | EIf(cond, e1, e2) ->
          fprintf ppf "if %a then@ %a@ else %a" print_e cond print_e e1 print_e e2
      | EInt x -> fprintf ppf "%d" x
      | EString x -> fprintf ppf "\"%s\"" x
      | ELet(pat, e, e1) -> fprintf ppf "let %a =@ %a@ in %a" print_pat pat print_e e print_e e1
      | EROLet(ro, pat, e, e1) -> 
          fprintf ppf "let {%a}@ %a =@ %a@ in %a" (print_list print_str) ro print_pat pat
          print_e e print_e e1
      | ETuple l -> fprintf ppf "(%a)" (print_list print_e) l
      | EArray l -> fprintf ppf "[|%a|]" (print_list print_e) l
      | EEmptyList -> print_str ppf "[]"
      | EOp(op, a, b) -> fprintf ppf "(%a@ %s@ %a)" print_e a (op_str op) print_e b
      | EVar x -> print_str ppf x
      | EApp(a, b) -> fprintf ppf "(%a %a)" print_e a print_e b
      | ECase(e, m) -> 
        fprintf ppf "case %a of" print_e e;
        List.iter (fun (p, e) -> fprintf ppf "@ | %a -> %a" print_pat p print_e e) m
    end;
    close_box ()
  in print_e

let rec print_val ppf (lazy v) =
  match v with
    | Eval.VInt x -> print_int ppf x
    | Eval.VString x -> print_str ppf x
    | Eval.VTuple l -> fprintf ppf "(%a)" (print_list print_val) l
    | Eval.VEmptyList
    | Eval.VCons (_, _) -> fprintf ppf "[%a]" (print_list print_val) 
                                   (Eval.to_list (Lazy.from_val v))
    | Eval.VArray l -> fprintf ppf "[|%a|]" (print_list print_val) (Array.to_list l)
    | Eval.VFunc _ -> print_str ppf "<fun>"
    | Eval.VConstr (n, Some c) -> fprintf ppf "(%s %a)" n print_val c
    | Eval.VConstr (n, None) -> print_str ppf n

let print_endline f ppf x = fprintf ppf "%a@." f x

let print_type = print_endline print_typ
let print_expr = print_endline print_exp
let print_pattern = print_endline print_pat
let print_value = print_endline print_val