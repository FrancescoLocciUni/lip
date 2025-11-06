open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* ----------------------------------- BIG STEP per espressione *)

(* eval_expr : state -> expr -> memval *)
let rec eval_expr (st : state) = function
  | True -> Bool true
  | False -> Bool false
  
  | Var x -> (match ((topenv st) x) with
    | BVar l | IVar l -> getmem st l
  )
  | Const c -> Int c
  
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
    | Int a1, Int a2 -> Int (a1 + a2)
    | _ -> raise NoRuleApplies
  )
  | Sub (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Int a1, Int a2 -> Int (a1 - a2)
    | _ -> raise NoRuleApplies
  )
  | Mul (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Int a1, Int a2 -> Int (a1 * a2)
    | _ -> raise NoRuleApplies
  )
  
  | Eq (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Int a1, Int a2 -> Bool (a1 = a2)
    | _ -> raise NoRuleApplies
  )
  | Leq (e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
    | Int a1, Int a2 -> Bool (a1 <= a2)
    | _ -> raise NoRuleApplies
  )
;;



(* ----------------------------------- SMALL STEP per comandi *)

(* let rec eval_decl1 (e:env) (dl:decl list) (fl:loc) = match dl with
  | [] -> e1
  | IntVar d :: t -> eval_decl1( bind_env e d fl ) t (IVar 1)
  | BoolVar d :: t -> eval_decl1( bind_env e d fl ) t (BVar 1)
;; *)

(* bind : state -> ide -> exprval -> state *)
let bind st x v : state = fun y -> if x = y then v else st y;;

(* eval_decl : state -> decl list -> state *)
let rec eval_decl (st:state, dl:decl list) =
  | [] -> st
  | BoolVar d | IntVar d :: t -> let t' = bind(st, d, v) 
      in { envstack : (setenv ((getenv st)::(getenv t'))); memory : getmem st; firstloc : setloc( (getloc st) + 1 ) }
;;


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


