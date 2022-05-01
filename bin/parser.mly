%token <int> INT 
%token <string> LOC
%token <bool> BOOL 

%token K_END
%token K_IF K_THEN K_ELSE 
%token K_WHILE K_DO
%token K_PRINT K_READ K_SKIP
%token EOF

%token SEMICOLON L_PAREN R_PAREN

%token ASSIGN

(* Operators *, +, -, <, >, <=, >=, ! *)
%token EQ GT LT GTE LTE NOT AND_AND OR_OR


%token TIMES
%token PLUS
%token MINUS


/* Precedence and associativity specification */
// %nonassoc  K_IF
// %nonassoc  K_ELSE

// %right    ASSIGN

%left     OR_OR
%left     AND_AND
// %left     EQ
// %nonassoc LT GT GTE LTE

%left     PLUS MINUS
%left     TIMES 

%start <Ast.program> prog

%%

(* Parsing rules *)

prog :
    | c = command EOF { c }

aexp:
    | id = LOC {
        Ast.Variable(id)
    }
    | n = INT {
        Ast.ILiteral(n)
    }
    | a0 = aexp PLUS a1 = aexp {
        Ast.Sum(a0, a1)
    }
    | a0 = aexp MINUS a1 = aexp {
        Ast.Sub(a0, a1)
    }
    | a0 = aexp TIMES a1 = aexp {
        Ast.Times(a0, a1)
    }
    | K_READ {
        Ast.Read
    }
    | L_PAREN e = aexp R_PAREN {
        e
    }

bexp:
    | b = BOOL {
        Ast.BLiteral(b)
    }
    | a0 = aexp EQ a1 = aexp {
        Ast.Eq(a0, a1)
    }
    | a0 = aexp LT a1 = aexp {
        Ast.Lt(a0, a1)
    }
    | a0 = aexp LTE a1 = aexp {
        Ast.Lte(a0, a1)
    }
    | a0 = aexp GT a1 = aexp {
        Ast.Gt(a0, a1)
    }
    | a0 = aexp GTE a1 = aexp {
        Ast.Gte(a0, a1)
    }
    | NOT b0 = bexp {
        Ast.Not(b0)
    }
    | b0 = bexp AND_AND b1 = bexp {
        Ast.And(b0, b1)
    }
    | b0 = bexp OR_OR b1 = bexp {
        Ast.Or(b0, b1)
    }
    | L_PAREN e = bexp R_PAREN {
        e
    }

command:
    | K_SKIP SEMICOLON { Ast.Skip }
    | l = LOC ASSIGN a0 = aexp SEMICOLON {                                            (* X := n *)
        Ast.Assign(l, a0)
    }
    | K_PRINT a0 = aexp SEMICOLON {
        Ast.Print(a0)
    }
    | c0 = command c1 = command {                                 (* c0; c1 *)
        Ast.Seq(c0, c1)
    }
    | K_IF b0 = bexp K_THEN bthen = command K_END {  (* if b then c0 end *)
        Ast.IfThen(b0, bthen, (Ast.Skip))
    }
    | K_IF b0 = bexp K_THEN bthen = command K_ELSE belse = command K_END {  (* if b then c0 else c1 end *)
        Ast.IfThen(b0, bthen, belse)
    }
    | K_WHILE b0 = bexp K_DO bwhile = command K_END {
        Ast.While(b0, bwhile)
    }