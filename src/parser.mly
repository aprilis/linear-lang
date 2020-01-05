%token <int> INT
%token <string> STRING
%token <string> VAR_ID
%token <string> CONSTR_ID
%token TYPE FORALL
%token FUN ARROW LIN_ARROW
%token LET BE IN
%token IF THEN ELSE
%token CASE OF
%token DOT QUEST
%token COLON COMMA BAR EXCL
%token LPAR RPAR LARRAY RARRAY LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS MULT DIV GT LT GEQ LEQ EQ NEQ AND OR CONS
%token WILD
%token EOF

%right CONS
%left AND OR
%left PLUS MINUS
%left MULT
%right ARROW LIN_ARROW

%start <Types.prog> prog_eof
%start <string Types.typ> type_eof
%start <Types.type_def> type_def_eof

%%

prog_eof: types = type_def*; e = expr; EOF  {(types, e)}
type_eof: t = var_typ; EOF                  {t}
type_def_eof: t = type_def; EOF             {t}

expr:
  | FUN; LPAR; x = pat; COLON; t = var_typ; RPAR; lin = arrow; e = expr       
                                                            {Types.EFun (lin, x, t, e)}
  | LET; ylist = braced_list; x = pat; BE; e1 = expr; IN; e2 = expr 
                                                            {Types.EROLet (ylist, x, e1, e2)}
  | LET; x = pat; BE; e1 = expr; IN; e2 = expr              {Types.ELet (x, e1, e2)}
  | CASE; e = expr; OF; boption(BAR); matches = separated_nonempty_list(BAR, match_item)
                                                            {Types.ECase (e, matches)}
  | IF; e = expr; THEN; e1 = expr; ELSE; e2 = expr          {Types.EIf (e, e1, e2)}
  | e = term                                                {e}

term:
  | e1 = term; o = op; e2 = term    {Types.EOp (o, e1, e2)}
  | es = app                        {es}

app: atoms = atom+ {
  let h::t = atoms in
  List.fold_left (fun x y -> Types.EApp (x, y)) h t
}

atom:
  | LPAR; e = expr; RPAR                                    {e}
  | LPAR; items = separated_list(COMMA, expr); RPAR         {Types.ETuple (items)}
  | LBRACKET; items = separated_list(COMMA, expr); RBRACKET {
      List.fold_right (fun x y -> Types.EOp(Types.OCons, x, y)) items Types.EEmptyList
    }
  | LARRAY; items = separated_list(COMMA, expr); RARRAY     {Types.EArray (items)}
  | n = INT                                                 {Types.EInt (n)}
  | s = STRING                                              {Types.EString (s)}
  | x = id                                                  {Types.EVar (x)}

braced_list: LBRACE; l = separated_list(COMMA, id); RBRACE {l}

%inline op:
  | PLUS    {Types.OPlus}
  | MINUS   {Types.OMinus}
  | MULT    {Types.OMult}
  | DIV     {Types.ODiv}
  | GT      {Types.OGt}
  | LT      {Types.OLt}
  | GEQ     {Types.OGeq}
  | LEQ     {Types.OLeq}
  | EQ      {Types.OEq}
  | NEQ     {Types.ONeq}
  | AND     {Types.OAnd}
  | OR      {Types.OOr}
  | CONS    {Types.OCons}

arrow:
  | ARROW     {false}
  | LIN_ARROW {true}

pat: 
  | x = VAR_ID                                              {Types.PVar (x)}
  | WILD                                                    {Types.PWild}
  | LPAR; items = separated_list(COMMA, pat); RPAR          {Types.PTuple (items)}
  | LBRACKET; items = separated_list(COMMA, pat); RBRACKET  {
      List.fold_right (fun x y -> Types.PCons (x, y)) items Types.PEmptyList
    }
  | h = pat; CONS; t = pat                                  {Types.PCons (h, t)}
  | lin = boption(EXCL); constr = CONSTR_ID; p = pat?       {Types.PConstr (lin, constr, p)}

typ:
  | x = typ; lin = arrow; y = typ                           {Types.TFunc (lin, x, y)}
  | t = typ_                                                {t}

typ_:
  | lin = boption(EXCL); x = id                             {Types.TPrim (lin, x)}
  | LPAR; t = typ; RPAR                                     {t}
  | LPAR; items = separated_list(COMMA, typ); RPAR          {Types.TTuple (items)}
  | LBRACKET; x = typ; RBRACKET                             {Types.TList (x)}
  | lin = boption(EXCL); LARRAY; x = typ; RARRAY            {Types.TArray (lin, x)}

var_typ: v = type_var_list; t = typ {
  t |> Types.map (fun t ->
    match t with
      | Types.TPrim (t, x) when List.mem_assoc x v ->
        if t then failwith @@ "Cannot use variable type with exclamation: !" ^ x
        else Types.TVar (List.assoc x v, x)
      | _ -> t)
}

type_var_list:
  | {[]}
  | FORALL; v = separated_nonempty_list(COMMA, type_var); DOT {v}

type_var:
  | QUEST; x = id  {(x, true)}
  | x = id         {(x, false)}

match_item: p = pat; ARROW; e = expr                             {(p, e)}

type_def: TYPE; lin = boption(EXCL); x = id; BE; items = separated_nonempty_list(BAR, type_option)
                                                            {Types.TypeDef (lin, x, items)}
type_option: x = CONSTR_ID; t = preceded(OF, typ)?            {(x, t)}

id:
  | x = VAR_ID    {x}
  | x = CONSTR_ID {x}