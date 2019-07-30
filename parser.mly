%{
  open Ast
%}

%token EOF

%token BREAK
%token CONTINUE
%token DEF
%token EFFECT
%token ELSE
%token EXCEPTION
%token FOR
%token IF
%token IN
%token MATCH
%token OF
%token PERFORM
%token REC
%token RETURN
%token TYPE
%token VAL
%token WHILE

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token LBRACKBAR
%token BARRBRACK
%token LBRACEBAR
%token BARRBRACE

%token BAR
%token COMMA
%token COLON
%token COLONCOLON
%token DOT
%token EQ
%token EQEQ
%token EQGT
%token INTERRO
%token SEMI
%token STAR

%token <string> INT
%token <string> LIDENT
%token <string> UIDENT

%nonassoc IF
%nonassoc ELSE
%nonassoc WHILE RETURN

%left SEMI
%nonassoc VAL

%left EQEQ
%left DOT

%start program
%type <Ast.program> program

%%

located(X):
    | x = X { { loc = { start_pos = $startpos; end_pos = $endpos } ; txt = x } }

separated_list2(DELIM, X):
  | x1 = X; DELIM; x2 = X { [x1; x2] }
  | x = X; DELIM; l = separated_list2(DELIM, X) { x :: l }

simple_expr:
    | e = located(simple_expr_desc) { e }

expr:
    | e = located(expr_desc) { e }

%inline simple_expr_desc:
    | LPAREN; e = expr; RPAREN { e.txt }
    | x = located(LIDENT) { EVar x }
    | e = simple_expr; LPAREN;
          l = separated_list(COMMA, function_argument); RPAREN
       { ECall (e, l) }
    | BREAK { EBreak }
    | CONTINUE { EContinue }
    | LBRACK; l = separated_list(COMMA, simple_expr); RBRACK { EList l }
    | LBRACKBAR; l = separated_list(COMMA, simple_expr); BARRBRACK { EArray l }
    | LPAREN; RPAREN { EUnit }
    | LPAREN; l = separated_list2(COMMA, simple_expr); RPAREN { ETuple l }
    | IF; e1 = simple_expr; LBRACE; e2 = expr; RBRACE { EIf (e1, e2, None) }
    | IF; e1 = simple_expr; LBRACE; e2 = expr; RBRACE;
      ELSE; LBRACE; e3 = expr; RBRACE
       { EIf (e1, e2, Some e3) }

%inline expr_desc:
    | e = simple_expr_desc { e }
    | VAL; r = rec_flag; x = located(LIDENT); EQ;
      e1 = simple_expr; SEMI; e2 = expr
       { EVal (x, r, e1, e2) }
    | DEF; r = rec_flag; x = located(LIDENT);
      LPAREN; l = separated_list(COMMA, located(LIDENT)); RPAREN;
      LBRACE; e1 = expr; RBRACE; e2 = expr
       { EDef (x, r, l, e1, e2) }
    | e1 = simple_expr; SEMI; e2 = expr { ESequence (e1, e2) }
    | FOR; x = located(LIDENT); IN; e1 = expr; LBRACE; e2 = expr; RBRACE
       { EFor (x, e1, e2) }
    | WHILE; e1 = expr; LBRACE; e2 = expr; RBRACE { EWhile (e1, e2) }
    | RETURN; e = simple_expr { EReturn e }

function_argument:
    | INTERRO         { None }
    | e = simple_expr { Some e }

rec_flag:
    | /* nothing */ { NonRecursive }
    | REC           { Recursive }

decl:
    | d = located(decl_desc) { d }

decl_desc:
    | VAL; r = rec_flag; x = located(LIDENT); EQ; e = expr
       { DVal (x, r, e) }
    | DEF; r = rec_flag; x = located(LIDENT);
      LPAREN; l = separated_list(COMMA, located(LIDENT)); RPAREN;
      LBRACE; e = expr; RBRACE
       { DDef (x, r, l, e) }

program:
    | l = list(decl); EOF { l }
