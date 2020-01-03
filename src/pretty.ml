open Types
open Util
open Format

let printStr ppf s = fprintf ppf "%s" s
let printInt ppf s = fprintf ppf "`%d`" s

let lpar ppf yes = if yes then printStr ppf "("
let rpar ppf yes = if yes then printStr ppf ")"

let printList f ppf l =
  match l with
    | h::t ->
      f ppf h;
      List.iter (fprintf ppf ",@ %a" f) t
    | [] -> ()

let printTyp ppf t = 
  let vars = Types.fold List.cons t [] |>
    Util.filter_map (function 
      | _, TVar x -> Some ("?" ^ x) 
      | _, TNonLinVar x -> Some x 
      | _ -> None) in
  printList printStr ppf vars;
  if vars <> [] then fprintf ppf "@ . ";
  let rec aux rarrow ppf t =
    open_hovbox 1;
    begin match t with
    | true, TFunc (x, y) ->
        lpar ppf rarrow;
        fprintf ppf "%a -o %a" (aux true) x (aux false) y;
        rpar ppf rarrow
    | lin, tt ->
      if lin then printStr ppf "!";
      match tt with
        | TArray s -> fprintf ppf "[|%a|]" (aux false) s
        | TFunc (x, y) -> 
            lpar ppf rarrow;
            fprintf ppf "%a@ ->@ %a" (aux true) x (aux false) y;
            rpar ppf rarrow
        | TList s -> fprintf ppf "[%a]" (aux false) s
        | TPrim s
        | TVar s
        | TNonLinVar s -> printStr ppf s
        | TTuple l -> fprintf ppf "(%a)" (printList (aux false)) l
    end;
    close_box ()
  in aux false ppf t

let arrowStr lin = if lin then "-o" else "->"
let opStr op = 
  match op with
    | OAnd -> "&&"
    | OCons -> "::"
    | OMinus -> "-"
    | OMult -> "*"
    | OOr -> "||"
    | OPlus -> "+"

let printExpr f = 
  let rec printPat ppf p =
    match p with
      | PVar x -> f ppf x
      | PWild -> printStr ppf "_"
      | PTuple l -> fprintf ppf "(%a)" (printList printPat) l
      | PCons(h, t) -> fprintf ppf "%a::%a" printPat h printPat t
      | PEmptyList -> printStr ppf "[]"
      | PConstr(c, None) -> printStr ppf c
      | PConstr(c, Some p) -> fprintf ppf "%s %a" c printPat p
  in let rec printE ppf e =
    open_hovbox 1;
    begin match e with
      | EFun(lin, pat, typ, e) ->
          fprintf ppf "fun (%a :@ %a) %s@ %a" printPat pat printTyp typ (arrowStr lin) printE e
      | EIf(cond, e1, e2) ->
          fprintf ppf "if %a then@ %a@ else %a" printE cond printE e1 printE e2
      | EInt x -> fprintf ppf "%d" x
      | EString x -> fprintf ppf "\"%s\"" x
      | ELet(pat, e, e1) -> fprintf ppf "let %a =@ %a@ in %a" printPat pat printE e printE e1
      | EROLet(ro, pat, e, e1) -> 
          fprintf ppf "let {%a}@ %a =@ %a@ in %a" (printList f) ro printPat pat
          printE e printE e1
      | ETuple l -> fprintf ppf "(%a)" (printList printE) l
      | EArray l -> fprintf ppf "[|%a|]" (printList printE) l
      | EEmptyList -> printStr ppf "[]"
      | EOp(op, a, b) -> fprintf ppf "(%a@ %s@ %a)" printE a (opStr op) printE b
      | EVar x -> f ppf x
      | EApp(a, b) -> fprintf ppf "(%a %a)" printE a printE b
      | ECase(e, m) -> 
        fprintf ppf "case %a of" printE e;
        List.iter (fun (p, e) -> fprintf ppf "@ | %a -> %a" printPat p printE e) m
    end;
    close_box ()
  in printE

let printEndline f ppf x = fprintf ppf "%a@." f x

let printType = printEndline printTyp
let printIntExpr = printEndline (printExpr printInt)
let printStringExpr = printEndline (printExpr printStr)