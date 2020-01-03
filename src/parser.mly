%token <int> INT
%token <string> STRING
%token <string> VAR_ID
%token <string> CONSTR_ID
%token TYPE
%token FUN ARROW LIN_ARROW
%token LET BE IN
%token IF THEN ELSE
%token CASE OF
%token DOT QUEST
%token COLON COMMA BAR EXCL
%token LPAR RPAR LARRAY RARRAY LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS MULT AND OR CONS
%token WILD
%token EOF

%right CONS
%left AND OR
%left PLUS MINUS
%left MULT
%right ARROW LIN_ARROW

%start <Types.prog> prog

%%

prog: types = type_def*; e = expr; EOF {(types, e)}

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
  | constr = CONSTR_ID; p = pat?                            {Types.PConstr (constr, p)}

typ:
  | x = typ; lin = arrow; y = typ                           {(lin, Types.TFunc (x, y))}
  | t = typ_                                                {t}

typ_:
  | EXCL; t = typ_                                          {let (_, tt) = t in (true, tt)}
  | x = id                                                  {(false, Types.TPrim (x))}
  | LPAR; t = typ; RPAR                                     {t}
  | LPAR; items = separated_list(COMMA, typ); RPAR          {(false, Types.TTuple (items))}
  | LBRACKET; x = typ; RBRACKET                             {(false, Types.TList (x))}
  | LARRAY; x = typ; RARRAY                                 {(false, Types.TArray (x))}

var_typ: v = type_var_list; t = typ {
  Types.map (fun t ->
    match t with
      | Types.TPrim x when List.mem (true, x) v ->
          Types.TVar x
      | Types.TPrim x when List.mem (false, x) v ->
          Types.TNonLinVar x
      | _ -> t
  ) t
}

type_var_list:
  | {[]}
  | v = separated_nonempty_list(COMMA, type_var); DOT {v}

type_var:
  | QUEST; x = id  {(true, x)}
  | x = id         {(false, x)}

match_item: p = pat; ARROW; e = expr                             {(p, e)}

type_def: TYPE; lin = boption(EXCL); x = id; BE; items = separated_list(BAR, type_option)
                                                            {Types.TypeDef (lin, x, items)}
type_option: x = id; t = preceded(COLON, typ)?              {(x, t)}

id:
  | x = VAR_ID    {x}
  | x = CONSTR_ID {x}