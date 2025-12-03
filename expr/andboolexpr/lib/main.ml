open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not(e) -> "not " ^ string_of_boolexpr e
  | And(e1,e2) -> string_of_boolexpr e1 ^ " and " ^ string_of_boolexpr e2
  | Or(e1,e2) -> string_of_boolexpr e1 ^ " or " ^ string_of_boolexpr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* --------------------------------------semantica Small Step *)


exception NoRuleApplies
  
let rec trace1 (e:boolExpr) = match e with
  (* if *)
  | If(True,e1,e2) -> e1
  | If(False,e1,e2) -> e2
  | If(e1,e2,e3) -> If(trace1 e1, e2, e3)
  (* not *)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> Not (trace1 e1)
  (* and *)
  | And(True, e1) -> e1
  | And(False, e1) -> False
  | And(e1, e2) -> And (trace1 e1, e2)
  (* or *)
  | Or(True, e1) -> True
  | Or(False, e1) -> e1
  | Or(e1, e2) -> Or (trace1 e1, e2)
  (* True/False *)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let string_of_val = function
    Some b -> string_of_bool b
  | None -> "None"


(* --------------------------------------semantica Big Step *)

  
let rec eval (e:boolExpr) = match e with
  | True -> true
  | False -> false
  | If(b1,b2,b3) -> if eval b1 then eval b2 else eval b3
  | And(b1,b2) -> (eval b1) && (eval b2)
  | Or(b1,b2) -> (eval b1) || (eval b2)
  | Not(b1) -> not (eval b1)
;;