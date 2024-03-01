%{
    type exp = 
        | Add of exp * exp
        | Sub of exp * exp
        | Mul of exp * exp
        | Div of exp * exp
        | Pow of exp * exp 
        | Sin of exp
        | Cos of exp
        | Tan of exp
        | Sec of exp
        | Csc of exp
        | Cot of exp
        | Exp of exp
        | Minus of exp
        | Num of int
        | Log of exp
        | X
        | Const of string
%}

%token  X
%token <int> INT
%token <string> CONST
%token ADD SUB MUL DIV POW 
%token SIN COS TAN SEC CSC COT 
%token LOG EXP
%token RPAREN LPAREN
%token EOF

%start e

%type <exp> e a m p n


%%

e : 
    a { $1 }
    | a ADD e { Add($1, $3) }
    | a SUB e { Sub($1, $3) }

a:
    m { $1 }
    | m MUL a { Mul($1, $3) }
    | m DIV a { Div($1, $3) }

m:
    p { $1 }
    | p POW m  { Pow($1, $3) }

p : 
    LPAREN e RPAREN { $2 }
    | n { $1}
    | SIN p { Sin($2) }
    | COS p { Cos($2) }
    | SUB p { Minus($2) }
    | LOG p { Log($2) }
    | TAN p { Tan($2) }
    | SEC p { Sec($2) }
    | CSC p { Csc($2) }
    | COT p { Cot($2) }
    | EXP p { Exp($2) }
    | X { X }

n : 
    | INT { Num($1) }
    | CONST { Const($1) }
    // | LOG INT { Log($2) }
