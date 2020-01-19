%token <int> INT
%token <char> CHAR
%token <string> STRING
%token <string> VAR_ID
%token <string> CONSTR_ID
%token TYPE
%token FUN ARROW LIN_ARROW
%token USE LET BE IN
%token IF THEN ELSE
%token CASE OF
%token DOT QUEST
%token SEMICOLON COLON COMMA BAR EXCL
%token LPAR RPAR LARRAY RARRAY LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS MULT DIV GT LT GEQ LEQ EQ NEQ AND OR CONS CONCAT PIPE
%token WILD
%token EOF

%right SEMICOLON
%left PIPE
%right CONCAT
%right CONS
%left AND OR
%left GT LT GEQ LEQ EQ NEQ
%left PLUS MINUS
%left MULT DIV
%right ARROW LIN_ARROW

%start <Types.prog> prog_eof
%start <string Types.typ> type_eof
%start <Types.type_def> type_def_opt_eof

%%

prog_eof: types = type_def*; ue = use; EOF  {let (u, e) = ue in (types, u, e)}
type_eof: t = var_typ; EOF                  {t}
type_def_opt_eof: t = type_def_opt; EOF     {t}

use:
  | e = expr                                                  {([], e)}
  | USE; v = separated_nonempty_list(COMMA, id); IN; e = expr {(v, e)}

expr:
  | FUN; v = type_var_list; LPAR; x = pat; COLON; t = typ; RPAR; lin = arrow; e = expr       
                                                            {Types.EFun (lin, v, x, t, e)}
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
  | LBRACKET; items = separated_list(COMMA, expr); RBRACKET {Types.unroll_list items}
  | LARRAY; items = separated_list(COMMA, expr); RARRAY     {Types.EArray (items)}
  | n = INT                                                 {Types.EInt (n)}
  | c = CHAR                                                {Types.EChar (c)}
  | s = STRING                                              {String.to_seq s |> List.of_seq
                                                             |> List.map (fun c -> Types.EChar (c))
                                                             |> Types.unroll_list}
  | x = id                                                  {Types.EVar (x)}
  | EXCL; x = id                                            {Types.EVar ("!" ^ x)}

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
  | CONCAT  {Types.OConcat}
  | SEMICOLON   {Types.OSemicolon}
  | PIPE        {Types.OPipe}

arrow:
  | ARROW     {false}
  | LIN_ARROW {true}

pat: 
  | x = VAR_ID                                              {Types.PVar (x)}
  | WILD                                                    {Types.PWild}
  | LPAR; items = separated_list(COMMA, pat); RPAR          {Types.PTuple (items)}
  | LBRACKET; items = separated_list(COMMA, pat); RBRACKET  {
      List.fold_right (fun x y -> Types.PCons(x, y)) items Types.PEmptyList
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
        Types.TVar ((if t then List.assoc x v else NonLin), x)
      | _ -> t)
}

type_var_list:
  | {[]}
  | LT; v = separated_nonempty_list(COMMA, type_var); GT {v}

type_var: l = var_linearity; x = id  {(x, l)}
var_linearity:
  | {Types.NonLin}
  | EXCL {Types.Lin}
  | QUEST {Types.AnyLin}

match_item: p = pat; ARROW; e = expr                             {(p, e)}

type_def: TYPE; lin = boption(EXCL); x = id; BE; items = separated_nonempty_list(BAR, type_option)
                                                            {Types.TypeDef (lin, x, items)}
type_option: x = CONSTR_ID; t = preceded(OF, typ)?            {(x, t)}

type_def_opt:
  | tp = type_def                     {tp}
  | TYPE; lin = boption(EXCL); x = id {Types.TypeDef (lin, x, [])}

id:
  | x = VAR_ID    {x}
  | x = CONSTR_ID {x}