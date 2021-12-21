type loc = string

type aExp =
  | Int of int
  | Loc of loc
  | Addition of (aExp * aExp)
  | Subtraction of (aExp * aExp)
  | Multiplication of (aExp * aExp)

type bExp =
  | True
  | False
  | Equal of (aExp * aExp)
  | Leq of (aExp * aExp)
  | Not of bExp
  | And of (bExp * bExp)
  | Or of (bExp * bExp)

type com =
  | Skip
  | Assign of (loc * aExp)
  | Seq of (com * com)
  | IfThenElse of (bExp * com * com)
  | While of (bExp * com)

type state = (loc, int) Hashtbl.t

let cmpAExp e0 e1 = 
  match (e0, e1) with
  | (Int(n1), Int(n2)) -> n1 == n2
  | (Loc(l1), Loc(l2)) -> l1 == l2
  | (Addition(a1, a2), Addition(b1, b2)) -> a1 == b1 && a2 == b2
  | (Subtraction(a1, a2), Subtraction(b1, b2)) -> a1 == b1 && a2 == b2
  | (Multiplication(a1, a2), Multiplication(b1, b2)) -> a1 == b1 && a2 == b2
  | _ -> false

let lookup loc state =
  Hashtbl.find state loc

let rec evalAExp aExp state =
  match aExp with
  | Int(n) -> n
  | Loc(l) -> lookup l state
  | Addition(a1, a2) -> evalAExp a1 state +  evalAExp a2 state
  | Subtraction(a1, a2) -> evalAExp a1 state - evalAExp a2 state
  | Multiplication(a1, a2) -> evalAExp a1 state * evalAExp a2 state

let rec evalBExp bExp state =
  match bExp with
  | True -> true
  | False -> false
  | Equal(a1, a2) -> evalAExp a1 state == evalAExp a2 state
  | Leq(a1, a2) -> evalAExp a1 state <= evalAExp a2 state
  | Not(b) -> not (evalBExp b state)
  | And(b1, b2) -> evalBExp b1 state && evalBExp b2 state
  | Or(b1, b2) -> evalBExp b1 state || evalBExp b2 state

let rec evalCom com state =
  match com with
  | Skip -> state
  | Assign(loc, aExp) -> 
    let n = evalAExp aExp state in Hashtbl.add state loc n;
    state
  | Seq(c1, c2) -> let nextState = evalCom c1 state in evalCom c2 nextState
  | IfThenElse(b, c1, c2) -> if (evalBExp b state) then evalCom c1 state else evalCom c2 state
  | While(b, c) -> if (evalBExp b state) then (evalCom c state) else evalCom Skip state