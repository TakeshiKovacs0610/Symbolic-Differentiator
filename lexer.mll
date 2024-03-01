{
    open Parser
}

(* write lexer for whitespaces integers plus and times *)

let white = [' ' '\t' '\n' '\r']+ 
let digit = ['0'-'9'] 
let ndigit = ['1'-'9'] 
let integer = ndigit digit* | '0'
let chr = ['a'- 'z']
let const = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let sin = "sin"
let cos = "cos"
let log = "log"
let tan = "tan"
let cot = "cot"
let sec = "sec"
let csc = "csc"

rule token = 
    parse
    | white { token lexbuf }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '+' { ADD}
    | '*' { MUL}
    | '(' { LPAREN}
    | ')' { RPAREN}
    | 'x' { X }
    | '-' { SUB }
    | '/' { DIV }
    | "e^"  { EXP}
    | sin { SIN }
    | cos { COS }
    | log { LOG }
    | tan { TAN }
    | cot { COT }
    | sec { SEC }
    | csc { CSC }
    | '^' { POW }
    | const { CONST (Lexing.lexeme lexbuf) }
    | eof { EOF }