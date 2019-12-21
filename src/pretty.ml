open Syntax
open Format

let printStr ppf s = fprintf ppf "%s" s

let lpar ppf yes = if yes then printStr ppf "("
let rpar ppf yes = if yes then printStr ppf ")"

let printList f ppf l =
  match l with
    | h::t ->
      f ppf h;
      List.iter (fprintf ppf ",@ %a" f) t
    | [] -> ()

let printType ppf t = 
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
        | TPrim s -> printStr ppf s
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

let rec printExpr ppf e = 
  open_hvbox 1;
  begin match e with
    | EFun(lin, pat, typ, e) ->
        fprintf ppf "fun (%a :@ %a) %s@ %a" printPat pat printType typ (arrowStr lin) printExpr e
    | EIf(cond, e1, e2) ->
        fprintf ppf "if %a then@ %a@ else %a" printExpr cond printExpr e1 printExpr e2
    | EInt x -> fprintf ppf "%d" x
    | EString x -> fprintf ppf "\"%s\"" x
    | ELet(pat, e, e1) -> fprintf ppf "let %a =@ %a@ in %a" printPat pat printExpr e printExpr e1
    | EROLet(pat, ro, e, e1) -> 
        fprintf ppf "let {%a}@ %a =@ %a@ in %a" (printList printStr) ro printPat pat
         printExpr e printExpr e1
    | EList l -> fprintf ppf "[%a]" (printList printExpr) l
    | ETuple l -> fprintf ppf "(%a)" (printList printExpr) l
    | EArray l -> fprintf ppf "[|%a|]" (printList printExpr) l
    | EOp(op, a, b) -> fprintf ppf "(%a@ %s@ %a)" printExpr a (opStr op) printExpr b
    | EVar x -> printStr ppf x
    | EApp(a, b) -> fprintf ppf "(%a %a)" printExpr a printExpr b
    | ECase(e, m) -> 
      fprintf ppf "case %a of" printExpr e;
      List.iter (fun (p, e) -> fprintf ppf "@ | %a -> %a" printPat p printExpr e) m
  end;
  close_box ()
and printPat ppf p =
  match p with
    | PVar x -> printStr ppf x
    | PWild -> printStr ppf "_"
    | PTuple l -> fprintf ppf "(%a)" (printList printPat) l
    | PCons(h, t) -> fprintf ppf "%a::%a" printPat h printPat t
    | PList l -> fprintf ppf "[%a]" (printList printPat) l
    | PConstr(c, p) -> fprintf ppf "%s %a" c printPat p

let printEndline ppf f x = fprintf ppf "%a@." f x