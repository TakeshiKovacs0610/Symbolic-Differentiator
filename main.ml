(* read input and print parse tree using Lexer and Parser*)
open Parser
open Lexer

exception TooComplex

(* 
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
        | Const of string
        | X

*)

let input_tree = let line = input_line stdin in
  let lexbuf = Lexing.from_string line in
  Parser.e Lexer.token lexbuf

let rec eval tree = match tree with
  | Num n -> Num 0 

  | X -> Num 1

  | Const s -> Num 0

  | Add (t1 , t2) -> let e1 = eval t1 and
    e2 = eval t2 in
    Add (e1, e2)

  | Sub (t1 , t2) -> let e1 = eval t1 and
    e2 = eval t2 in
    Sub (e1, e2)

  | Mul (t1 , t2) -> let e1 = eval t1 and
    e2 = eval t2 in
    Add(Mul(e1, t2), Mul(t1, e2))

  | Div (t1 , t2) -> let e1 = eval t1 and
    e2 = eval t2 in
    Div(Sub(Mul(e1, t2), Mul(t1, e2)), Pow(t2, Num 2))

  | Pow (t1 , t2) -> let e1 = eval t1 and e2  = eval t2 in
    let t3 = match t2 with
      | Num n -> Mul(Num n, Mul (Pow(t1, Num (n-1)), e1))
      | X -> Mul( tree, Add(Log t1, Div(e1,t1)) )  
      | _ -> Mul( tree, Add(Mul(e2 , Log t1), Mul (t2, Div(e1,t1))) )
    in t3

  | Log t -> let e = eval t in
    Div(e, t)

  | Sin t -> let e = eval t in
    Mul(e, Cos t)

  | Cos t -> let e = eval t in
    Mul( e, Minus(Sin t))    

  | Tan t -> let e = eval t in
    Mul(Pow(Sec t, Num 2), e)

  | Sec t -> let e = eval t in
    Mul(Mul(Sec t, Tan t), e)

  | Csc t -> let e = eval t in
    Mul(Minus(Csc t), e)

  | Cot t -> let e = eval t in
    Mul(Minus(Pow(Csc t, Num 2)), e)

  | Minus t -> let e = eval t in
    Minus e

  | Exp t -> let e = eval t in
    Mul(Exp t, e)



let rec simplify tree = match tree with
  | Num n -> Num n
  | X -> X
  | Const s -> Const s
  | Add (e1 , e2) -> let t1 = simplify e1 and t2 = simplify e2 in
    if t1 = Num 0 then t2 else if t2 = Num 0 then t1 else (*Add(t1, t2)*)
      let t3 = match (t1,t2) with
        | Num n1, Num n2 -> Num (n1 + n2)
        | _ -> Add(t1, t2)
      in t3
  | Sub (e1 , e2) -> let t1 = simplify e1 and t2 = simplify e2 in
    if t1 = Num 0 then Minus t2 else if t2 = Num 0 then t1 else (*Sub(t1, t2)*)
      let t3 = match (t1,t2) with
        | Num n1, Num n2 -> Num (n1 - n2)
        | _ -> Sub(t1, t2)
      in t3
  | Mul (e1 , e2) -> let t1 = simplify e1 and t2 = simplify e2 in
    if t1 = Num 0 || t2 = Num 0 then Num 0 else if t1 = Num 1 then t2 else if t2 = Num 1 then t1 else (*Mul(t1, t2)*)
      let t3 = match (t1,t2) with
        | Num n1, Num n2 -> Num (n1 * n2)
        | _ -> Mul(t1, t2)
      in t3
  | Div (e1 , e2) -> let t1 = simplify e1 and t2 = simplify e2 in
    if t1 = Num 0 then Num 0 else if t2 = Num 1 then t1 else (*Div(t1, t2)*)
      let t3 = match (t1,t2) with
        | Sin t, Cos t1 -> if (t1 = t) then Tan t else Div(t1, t2)
        | Cos t, Sin t1 -> if (t1 = t) then Cot t else Div(t1, t2)
        | Num 1, Sin t -> Csc t
        | Num 1, Cos t -> Sec t
        | _ -> Div(t1, t2)
      in t3

  | Pow (e1 , e2) -> let t1 = simplify e1 and t2 = simplify e2 in
    if t1 = Num 0 then Num 0 else if t2 = Num 1 then t1 else Pow(t1, t2)
  | Sin t -> Sin (simplify t)
  | Cos t -> Cos (simplify t)
  | Log t -> Log (simplify t)
  | Tan t -> Tan (simplify t)
  | Sec t -> Sec (simplify t)
  | Csc t -> Csc (simplify t)
  | Cot t -> Cot (simplify t)
  | Exp t -> Exp (simplify t)
  | Minus t -> let t' = match t with
    | Minus t1 -> t1
    | Num n -> Num (-n)
    | _ -> Minus (simplify t)
    in t'


let output_tree = simplify (eval input_tree)

let rec tree_to_expr = function
  | Num n -> string_of_int n
  | X -> "x"
  | Add (exp1, exp2) -> "(" ^ tree_to_expr exp1 ^ " + " ^ tree_to_expr exp2 ^ ")"
  | Sub (exp1, exp2) -> "(" ^ tree_to_expr exp1 ^ " - " ^ tree_to_expr exp2 ^ ")"
  | Mul (exp1, exp2) -> "(" ^ tree_to_expr exp1 ^ " * " ^ tree_to_expr exp2 ^ ")"
  | Div (exp1, exp2) -> "(" ^ tree_to_expr exp1 ^ " / " ^ tree_to_expr exp2 ^ ")"
  | Pow (exp1, exp2) -> "(" ^ tree_to_expr exp1 ^ " ^ " ^ tree_to_expr exp2 ^ ")"
  | Sin exp -> "sin(" ^ tree_to_expr exp ^ ")"
  | Cos exp -> "cos(" ^ tree_to_expr exp ^ ")"
  | Log exp -> "log(" ^ tree_to_expr exp ^ ")"
  | Minus exp -> "-(" ^ tree_to_expr exp ^ ")"
  | Tan exp -> "tan(" ^ tree_to_expr exp ^ ")"
  | Sec exp -> "sec(" ^ tree_to_expr exp ^ ")"
  | Csc exp -> "csc(" ^ tree_to_expr exp ^ ")"
  | Cot exp -> "cot(" ^ tree_to_expr exp ^ ")"
  | Exp exp -> "e^(" ^ tree_to_expr exp ^ ")"
  | Const s -> s

let () = Printf.printf "%s\n" (tree_to_expr output_tree)


