%token <int> INT
%token <string> STRING
%token <string> ID
%token TYPE
%token FUN ARROW LIN_ARROW
%token LET BE IN
%token IF THEN ELSE
%token CASE OF
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

%start <Syntax.prog> prog
%start <Syntax.typ> typ_eof

%%

typ_eof: t = typ; EOF {t}

prog: types = type_def*; e = expr; EOF {(types, e)}

expr:
  | FUN; LPAR; x = pat; COLON; t = typ; RPAR; lin = arrow; e = expr       
                                                            {Syntax.EFun (lin, x, t, e)}
  | LET; ylist = braced_list; x = pat; BE; e1 = expr; IN; e2 = expr 
                                                            {Syntax.EROLet (x, ylist, e1, e2)}
  | LET; x = pat; BE; e1 = expr; IN; e2 = expr              {Syntax.ELet (x, e1, e2)}
  | CASE; e = expr; OF; boption(BAR); matches = separated_nonempty_list(BAR, match_item)
                                                            {Syntax.ECase (e, matches)}
  | IF; e = expr; THEN; e1 = expr; ELSE; e2 = expr          {Syntax.EIf (e, e1, e2)}
  | e = term                                                {e}

term:
  | e1 = term; o = op; e2 = term    {Syntax.EOp (o, e1, e2)}
  | es = app                        {es}

app: atoms = atom+ {
  let h::t = atoms in
  List.fold_left (fun x y -> Syntax.EApp (x, y)) h t
}

atom:
  | LPAR; e = expr; RPAR                                    {e}
  | LPAR; items = separated_list(COMMA, expr); RPAR         {Syntax.ETuple (items)}
  | LBRACKET; items = separated_list(COMMA, expr); RBRACKET {Syntax.EList (items)}
  | LARRAY; items = separated_list(COMMA, expr); RARRAY     {Syntax.EArray (items)}
  | n = INT                                                 {Syntax.EInt (n)}
  | s = STRING                                              {Syntax.EString (s)}
  | x = ID                                                  {Syntax.EVar (x)}

braced_list: LBRACE; l = separated_list(COMMA, ID); RBRACE {l}

%inline op:
  | PLUS    {Syntax.OPlus}
  | MINUS   {Syntax.OMinus}
  | MULT    {Syntax.OMult}
  | AND     {Syntax.OAnd}
  | OR      {Syntax.OOr}
  | CONS    {Syntax.OCons}

arrow:
  | ARROW     {false}
  | LIN_ARROW {true}

pat: 
  | x = ID                                                  {Syntax.PVar (x)}
  | WILD                                                    {Syntax.PWild}
  | LPAR; items = separated_list(COMMA, pat); RPAR          {Syntax.PTuple (items)}
  | LBRACKET; items = separated_list(COMMA, pat); RBRACKET  {Syntax.PList (items)}
  | h = pat; CONS; t = pat                                  {Syntax.PCons (h, t)}
  | constr = ID; p = pat                                    {Syntax.PConstr (constr, p)}

typ:
  | x = typ; lin = arrow; y = typ                           {(lin, Syntax.TFunc (x, y))}
  | t = typ_                                                {t}

typ_:
  | EXCL; t = typ_                                          {let (_, tt) = t in (true, tt)}
  | x = ID                                                  {(false, Syntax.TPrim (x))}
  | LPAR; t = typ; RPAR                                     {t}
  | LPAR; items = separated_list(COMMA, typ); RPAR          {(false, Syntax.TTuple (items))}
  | LBRACKET; x = typ; RBRACKET                             {(false, Syntax.TList (x))}
  | LARRAY; x = typ; RARRAY                                 {(false, Syntax.TArray (x))}


match_item: p = pat; ARROW; e = expr                             {(p, e)}

type_def: TYPE; lin = boption(EXCL); x = ID; BE; items = separated_list(BAR, type_option)
                                                            {Syntax.TypeDef (lin, x, items)}
type_option: x = ID; t = preceded(COLON, typ)?              {(x, t)}