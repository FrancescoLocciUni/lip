open Ast

type exprval = Bool of bool | Nat of int

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* ------------------------------semantica SMALL_STEP *)

exception NoRuleApplies

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
  
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False    
  | IsZero(e) -> let e' = trace1 e in IsZero(e')    
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(* ------------------------------semantica BIG_STEP *)

exception TypeError;;

let rec eval (e:expr) : exprval = match e with
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0

  | If(e0, e1, e2) -> (match eval(e0) with
    | (Bool a) -> if a then eval(e1) else eval(e2)
    | _ -> raise TypeError
    )

  | And(e1, e2) -> (match (eval(e1), eval(e2)) with
    | (Bool a, Bool b) -> Bool (a && b)
    | _ -> raise TypeError)

  | Or(e1, e2) -> (match (eval(e1), eval(e2)) with
    | (Bool a, Bool b) -> Bool (a || b)
    | _ -> raise TypeError)
    
  | Not(e1) -> (match eval e1 with
    | Bool a -> Bool (not(a))
    | _ -> raise TypeError)
    
  | Pred(e1) -> (match eval e1 with
    | Nat n when n > 0 -> Nat (n - 1)
    | _ -> raise TypeError)

  | Succ(e1) -> (match eval e1 with
    | Nat n ->  Nat (n + 1)
    | _ -> raise TypeError)

  | IsZero(e1) -> (match eval e1 with
    | Nat n -> Bool (n == 0)
    | _ -> raise TypeError)
;;