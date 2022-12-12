%token BREAK DO ELSE ELSEIF END FALSE FOR FUNCTION IF IN LOCAL NIL
%token REPEAT RETURN THEN TRUE UNTIL WHILE EOF

%token COLON ":"
%token COMMA ","
%token DOT "."
%token DOTS "..."
%token EQUALS "="
%token SEMICOLON ";"

%token OPAREN "(" CPAREN ")"
%token OBRACE "{" CBRACE "}"
%token OSQUARE "[" CSQUARE "]"

%token ADD "+" SUB "-" MUL "*" DIV "/" POW "^" MOD "%"
%token CONCAT ".."
%token EQ "==" NE "~=" LT "<" LE "<=" GT ">" GE ">="
%token LEN "#"

%token AND OR
%token NOT

%token IDENT
%token STRING
%token INT
%token NUMBER
%token MNUMBER

%left OR
%left AND
%left "<" ">" "<=" ">=" "~=" "=="
%right ".."
%left "+" "-"
%left "*" "/" "%"
%right NOT
%right "^"

%start <unit> program
%start <unit> repl_exprs

%on_error_reduce
  name
  var

  expr_pow

  stmts
  if_clause(ELSEIF)

%%

let program := stmts ; EOF ; {()}

let repl_exprs :=
  | repl_exprs = sep_list1(",", expr) ; EOF
  ; {()}

let var := IDENT ; {()}

let arg :=
  | var ; {()}
  | "..." ; {()}

let args :=
  | "(" ; sep_list0(",", arg) ; ")"
  ; {()}

let name :=
  | var
  ; {()}
  | simple_expr ; "." ; IDENT
  ; {()}
  | simple_expr ; "[" ; expr ; "]"
  ; {()}

let simple_expr :=
  | name ; {()}
  | paren_open = "(" ; paren_expr = expr ; paren_close = ")"
  ; {()}
  | call ; {()}

let call :=
  | fn = simple_expr ; args = call_args
  ; {()}
  | obj = simple_expr ; colon = ":" ; meth = IDENT ; args = call_args
  ; {()}

let call_args :=
  | open_a = "(" ; args = sep_list0(",", expr) ; close_a = ")"
  ; {()}
  | STRING ; {()}
  | table  ; {()}

(* Expressions *)
let atom :=
  | simple_expr
  | table   ; <>
  | NIL     ; <>
  | TRUE    ; <>
  | FALSE   ; <>
  | "..."   ; <>
  | INT     ; <>
  | NUMBER  ; <>
  | MNUMBER ; <>
  | STRING  ; <>
  | FUNCTION ; args ; stmts ; END
  ; {()}

let expr :=
  | expr_pow

  | binop_lhs = expr ; binop_op = AND ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = OR  ; binop_rhs = expr ; {()}


  | binop_lhs = expr ; binop_op = "+" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "-" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "*" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "/" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "%" ; binop_rhs = expr ; {()}

  | binop_lhs = expr ; binop_op = ".." ; binop_rhs = expr ; {()}

  | binop_lhs = expr ; binop_op = "==" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "~=" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "<"  ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = "<=" ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = ">"  ; binop_rhs = expr ; {()}
  | binop_lhs = expr ; binop_op = ">=" ; binop_rhs = expr ; {()}

let expr_pow :=
  | atom
  | binop_lhs = expr_pow ; binop_op = "^" ; binop_rhs = expr_pow
  ; {()}

  | unop_op = "-" ; unop_rhs = expr_pow
  ; {()} %prec NOT
  | unop_op = "#" ; unop_rhs = expr_pow ; {()} %prec NOT
  | unop_op = NOT ; unop_rhs = expr_pow ; {()} %prec NOT

(* Tables *)
let table :=
  | table_open = "{" ; table_body = table_body ; table_close = "}"
  ; {()}

let table_sep := ";" | ","

let table_body :=
  | {()}
  | x = table_entry ; {()}
  | x = table_entry ; s = table_sep ; xs = table_body ; {()}

let table_entry :=
  | expr ; {()}
  | ident = IDENT ; eq = "=" ; value = expr ;{()}
  | open_k = "[" ; key = expr ; close_k = "]" ; eq = "=" ; value = expr
  ; {()}

(* Statements *)

let stmts := list(stmt)

let stmt :=
  | basic_stmt ; {()}
  | call ; {()}

let basic_stmt :=
  | ";" ; <>
  | BREAK ; <>

  | DO ; stmts ; END
  ; {()}

  | sep_list1(",", name) ; "=" ; sep_list1(",", expr)
  ; {()}

  | WHILE ; expr ; DO ; stmts ; END
  ; {()}

  | REPEAT ; stmts ; UNTIL ; expr
  ; {()}

  | if_clause(IF) ; list(if_clause(ELSEIF))
  ; option(x = ELSE ; y = stmts ; { (x, y) }) ; END
  ; {()}

  | FOR ; var ; "=" ; expr ; ","; expr
  ; option (x = "," ; y = expr ; { (x, y) }) ; DO ; stmts ; END
  ; {()}

  | FOR ; sep_list1(",", var) ; IN ; sep_list1(",", expr)
  ; DO ; stmts ; END
  ; {()}

  | LOCAL ; sep_list1(",", var)
  ; option(x = "=" ; y = sep_list1(",", expr) ; { (x, y) })
  ; {()}

  | LOCAL ; FUNCTION ; var ; args
  ; stmts ; END
  ; {()}

  | FUNCTION ; function_name; args
  ; stmts ; END
  ; {()}

  | RETURN ; sep_list0(",", expr)
  ; {()}

let function_name :=
  | var ; {()}
  | function_name ; "." ; IDENT ; {()}
  | function_name ; ":" ; IDENT  ; {()}

let if_clause(t) :=
  | t ; expr ; THEN ; stmts ;
  {()}

let sep_list1(separator, X) :=
  | X ; {()}
  | X; separator; sep_list1(separator, X)
  ; {()}

let sep_list0(separator, X) :=
  | {()}
  | sep_list1(separator, X) ; {()}
