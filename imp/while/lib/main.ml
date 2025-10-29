open Ast
open Types


(* -------------------------BIG STEP per espressioni *)
let rec eval_expr (st : state)  = function
  | True -> Bool true
  | False -> Bool false
  
  | Var x -> st x
  | Const c -> Nat c
  
  | Not e -> (match (eval_expr st e) with
    | Bool b -> Bool (not b)
    | _ -> raise NoRuleApplies
  )
  | And (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> raise NoRuleApplies
  )
  | Or (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> raise NoRuleApplies
  )
  
  | Add (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Nat a1, Nat a2 -> Nat (a1 + a2)
    | _ -> raise NoRuleApplies
  )
  | Sub (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Nat a1, Nat a2 -> Nat (a1 - a2)
    | _ -> raise NoRuleApplies
  )
  | Mul (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Nat a1, Nat a2 -> Nat (a1 * a2)
    | _ -> raise NoRuleApplies
  )
  
  | Eq (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Nat a1, Nat a2 -> Bool (a1 = a2)
    | _ -> raise NoRuleApplies
  )
  | Leq (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Nat a1, Nat a2 -> Bool (a1 <= a2)
    | _ -> raise NoRuleApplies
  )
;;



(* -------------------------SMALL STEP per comandi *)

(* bind : state -> ide -> exprval -> state *)
let bind st x v : state = fun y -> if x = y then v else st y;;


(* trace1 : conf -> conf *)
let rec trace1 (c:conf) = match c with
  | St _ -> raise NoRuleApplies
  | Cmd (c, st) -> ( match c with
    | Skip -> St st
    | Assign (name, e) -> (
      match eval_expr st e with
        | v -> St ( bind st name v )
    )
    | Seq (c1, c2) -> (
      match trace1 (Cmd (c1, st)) with
        | St st' -> Cmd ( c2, st' )
        | Cmd (c1', st') -> Cmd ( Seq (c1', c2), st' )
    )
    | If (e, c1, c2) -> (
      match eval_expr st e with
        | Bool true -> Cmd ( c1, st )
        | Bool false -> Cmd ( c2, st )
        | _ -> raise (UnboundVar "errore nella valutazione del condizionale if")
    )
    | While (e, c1) -> (
      match eval_expr st e with
        | Bool false -> St st
        | Bool true -> Cmd ( Seq(c1, While (e, c1)), st )
        | _ -> raise (UnboundVar "errore nella valutazione del ciclo while")
    )
  )
;;


(* -------------------------funzione trace *)

let rec trace_aux n c =
  if n <= 0 then [c] else (
  try
    let c' = trace1 c
    in c::(trace_aux (n-1) c')
  with NoRuleApplies -> [c]
  )
;;

let bottom = fun x -> raise (UnboundVar x)

(* trace : int -> cmd -> conf list *)
let trace n t = trace_aux n (Cmd(t,bottom))

