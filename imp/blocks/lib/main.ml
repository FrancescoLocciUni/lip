open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* BIG-STEP SEMANTICS (for expressions) *)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> (match ((topenv st) x) with BVar l | IVar l -> l) |> getmem st
  | Const n -> Int n
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1, Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1, Int n2) -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Mul")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1, Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          


(* SMALL-STEP SEMANTICS *)

(* creates a new empty environment with the new bindings from the declarations list *)
let rec eval_decl1 (e: env) (dl: decl list) (fl: loc) : env * loc =
  match dl with
  | [] -> (e, fl)
  | IntVar x :: t -> let e' = bind_env e x (IVar fl) in eval_decl1 e' t (fl + 1)
  | BoolVar x :: t -> let e' = bind_env e x (BVar fl) in eval_decl1 e' t (fl + 1);;

  
(* pushes the new environment on top of the stack *)
let eval_decl (st: state) (dl: decl list) : state =
  let e0 = topenv st in
  let fl0 = getloc st in
  let (e1, fl1) = eval_decl1 e0 dl fl0 in
  let envstack' = e1 :: (getenv st) in
  let st' = setenv st envstack' in
  setloc st' fl1

let rec trace1 = function
    St _ -> raise NoRuleApplies (* no more commands to be executed *)
  | Cmd(c,st) -> match c with (* if the command is a Skip, go to the next command *)
      | Skip -> St st
      | Assign(x,e) ->
          (* look at the bindings in the last environment and make the new assignment *)
          let mem = getmem st in
          let st' =
            match ((topenv st) x, eval_expr st e) with (* take the memory, add the assignment location -> value and change the state with this new memory*)
            | (IVar l, Int n) -> setmem st (bind_mem mem l (Int n)) (* if the variable is Int and it was assigned an Int value, make the assignment *)
            | (BVar l, Bool b) -> setmem st (bind_mem mem l (Bool b)) (* if the variable is Bool and it was assigned a Bool value, make the assignment *)
            | _ -> raise (TypeError "Assign")
          in St st'
      | Seq(c1,c2) ->
          (match trace1 (Cmd(c1,st)) with
           | St st1 -> Cmd(c2,st1)
           | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
      | If(e,c1,c2) ->
          (match eval_expr st e with
           | Bool true -> Cmd(c1,st)
           | Bool false -> Cmd(c2,st)
           | _ -> raise (TypeError "If"))
      | While(e,c) ->
          (match eval_expr st e with
           | Bool true -> Cmd(Seq(c,While(e,c)),st)
           | Bool false -> St st
           | _ -> raise (TypeError "While"))
      | Decl(dl, c) ->
          (* Alloca le dichiarazioni creando un nuovo frame *)
          let st' = eval_decl st dl in
          Cmd(Block c, st')
      | Block c' ->
          (* ONLY AT RUNTIME, this gets created when we encounter new declarations, this is only needed to remind the interpreter that it's a different block *)
          (* whenever we find {int x, int y ... commands} for example, we create a new block with the new declarations *)
          (match trace1 (Cmd(c', st)) with (* once the declarations have been made, therefore the new bindings, we are back in the previous block*)
           | St st1 ->
               let envstack' = popenv st1 in
               St (setenv st1 envstack')
           | Cmd(c'', st1) -> Cmd(Block c'', st1))

           
let rec trace_rec n t =
  if n <= 0 then [t]
  else
    try
      let t' = trace1 t in
      t :: (trace_rec (n-1) t')
    with NoRuleApplies -> [t]

let trace n c = trace_rec n (Cmd(c, state0))
