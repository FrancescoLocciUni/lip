open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

  

(* ----------------------------------- SMALL STEP per espressioni *)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies


let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false
;;


let rec trace1_expr st = function
  (* costanti booleane *)
  | True -> (True, st)
  | False -> (False, st)

  (* variabili e costanti *)
  | Var(x) -> ((Const (apply st x)), st)
  | Const(n) -> (Const(n), st)

  (* not *)
  | Not(True) -> (False, st)
  | Not(False) -> (True, st)
  | Not(e) -> let (e', st') = trace1_expr st e in (Not(e'), st')
  (* and *)
  | And(True,e) -> (e, st)
  | And(False,_) -> (False, st)
  | And(e1,e2) -> let (e1', st') = trace1_expr st e1 in (And(e1',e2), st')
  (* or *)
  | Or(True,_) -> (True, st)
  | Or(False,e) -> (e, st)
  | Or(e1,e2) -> let (e1', st') = trace1_expr st e1 in (Or(e1',e2), st')

  (* add *)
  | Add (Const n1, Const n2) -> (Const (n1 + n2), st)
  | Add(Const n,e) -> let (e', st') = trace1_expr st e in (Add(Const n,e'), st')
  | Add(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Add(e1', e2), st')  

  (* sub *)
  | Sub (Const n1, Const n2) -> (Const (n1 - n2), st)
  | Sub(Const n,e) -> let (e', st') = trace1_expr st e in (Sub(Const n,e'), st')
  | Sub(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Sub(e1', e2), st')

  (* mul *)
  | Mul (Const n1, Const n2) -> (Const (n1 * n2), st)
  | Mul(Const n,e) -> let (e', st') = trace1_expr st e in (Mul(Const n,e'), st')
  | Mul(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Mul(e1', e2), st')

  (* equals *)
  | Eq(Const n1, Const n2) -> ((if n1 = n2 then True else False), st)
  | Eq(Const n, e) -> let (e', st') = trace1_expr st e in (Eq(Const n, e'), st')
  | Eq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Eq(e1', e2), st')

  (* less than or equals *)
  | Leq(Const n1, Const n2) -> ((if n1 <= n2 then True else False), st)
  | Leq(Const n, e) -> let (e', st') = trace1_expr st e in (Leq(Const n, e'), st')
  | Leq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Leq(e1', e2), st')

  (* chiamata a funzione *)
  | Call(f, Const n) -> (match (topenv st) f with
    | IFun (x, c, e) ->
      let l = getloc st in
      let env' = bind (topenv st) x (IVar l) in
      let mem' = bind (getmem st) l n in
      let st' = { envstack = env'::(getenv st); memory = mem'; firstloc = l + 1} in
      (CallExec(c, e), st')
    | _ -> raise (TypeError "Sto cercando di richiamare qualcosa che non è funzione"))
  | Call(f, e) -> let (e', st') = trace1_expr st e in (Call(f, e'), st')

  (* Runtime: esecuzione e restituzione *)
  | CallExec(c, e) -> (match trace1_cmd(Cmd(c, st)) with
      | St st' -> ((CallRet(e)), st')
      | Cmd(c', st') -> (CallExec(c',e), st')
  )
  | CallRet(Const(n)) -> let st' = {envstack=(popenv st); memory=(getmem st); firstloc=(getloc st)} in (Const(n), st')
  | CallRet(e) -> let (e', st') = trace1_expr st e in (CallRet(e'), st') 


(* ----------------------------------- SMALL STEP per comandi *)


(* let rec trace1_cmd = function
  | St _ -> raise NoRuleApplies (* no more commands to be executed *)
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
        (match trace1_cmd (Cmd(c1,st)) with
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
  ;; *)


and trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,Const(n)) -> (match topenv st x with
        IVar l -> St ({envstack=getenv st;memory= bind (getmem st) l n;firstloc= getloc st})
      | _ -> failwith "improve err msg")
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st') 
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')
    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),st)

let rec sem_decl (e,l) = function
  | [] -> (e,l)
  | (IntVar x) :: ds ->
      let e' = bind e x (IVar l) in
      sem_decl (e', l+1) ds
  | (Fun(f,x,c,er)) :: ds ->
      let e' = bind e f (IFun(x,c,er)) in
      sem_decl (e', l) ds
;;

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]
;;

let trace n (Prog(d,c)) =
  let (e, l) = sem_decl (botenv, 0) d 
  in trace_rec n (Cmd(c,({envstack=[e];memory=botmem;firstloc=l})))