type expr 
  = Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  
  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr

  | If of expr * expr * expr

  | Id of string
  
  | Let of string * expr * expr
  | LetRec of string * expr * expr

  | App of expr * expr
  | Lambda of string * expr

  | Value of value

and value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
and environment = (string * value) list


let rec freevars (e:expr)  : string list =
  match e with
  | Value v -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2) -> freevars e1 @ freevars e2
  | Lt  (e1, e2) -> freevars e1 @ freevars e2
  | Eq  (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | If  (e1, e2, e3) -> freevars e1 @ freevars e2 @ freevars e3
  | Let (i, dexpr, body) ->
     freevars dexpr @ List.filter (fun i' -> i' <> i) (freevars body)
  | LetRec (i, dexpr, body) ->
     freevars dexpr @ List.filter (fun i' -> i' <> i) (freevars body)
  | Id i -> [i]
  | App (e1, e2) -> [] @ freevars e2 
  (*
  (List.filter (fun y -> not (List.mem y (freevars e2)))(freevars e1))@(freevars e2)
  *)
  | Lambda (i, exp) -> List.filter (fun i' -> i' <> i) (freevars exp)

let rec lookup (n:string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " not in scope"))
  | (n',v) :: rest when n = n' -> v
  | _ :: rest -> lookup n rest

let rec eval (env:environment) (e:expr) : value =
  match e with 
  | Value v -> v
  | Add (e1, e2) -> 
    ( match eval env e1, eval env e2 with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _, _ -> raise (Failure "incompatible type on Add")
    )
  | Sub (e1, e2) -> 
     (  match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Int (i1 - i2)
       | _, _ -> raise (Failure "incompatible type on Sub")
     )
  | Mul (e1, e2) ->
    (  match eval env e1, eval env e2 with
      | Int i1, Int i2 -> Int (i1 * i2)
      | _, _ -> raise (Failure "incompatible type on Mul")
    )
  | Div (e1, e2) ->
    (  match eval env e1, eval env e2 with
      | Int i1, Int i2 -> Int (i1 / i2)
      | _, _ -> raise (Failure "incompatible type on Div")
    )
  | Lt (e1, e2) -> 
     (  match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 < i2)
       | _, _ -> raise (Failure "incompatible type on Lt")
     )
  | Eq (e1, e2) ->
     (  match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool i1, Bool i2 -> Bool (i1 = i2)
       | _, _ -> raise (Failure "incompatible type on Eq")
     )
  | And (e1, e2) ->
     (  match eval env e1, eval env e2 with
       | Bool i1, Bool i2 -> Bool (i1 && i2)
       | _, _ -> raise (Failure "incompatible type on And")
     )
  | If (e1, e2, e3) ->
     (  match eval env e1, eval env e2, eval env e3 with
       | Bool i1, Bool i2, Bool i3 -> Bool ( if i1 then i2 else i3 )
       | Bool i1, Int i2, Int i3 -> Int ( if i1 then i2 else i3 )
       | _, _, _ -> raise (Failure "incompatible type on If")
     )
  | Id n -> lookup n env
  | Let (n, dexpr, body) -> (let dexpr_v = eval env dexpr in 
    			     let body_v = eval ((n,dexpr_v)::env) body  in
    			        body_v)
  | LetRec (i, dexpr, body) -> Int 55
  | App (e1, e2) -> let enva = [] in let body = eval enva e1 in( 
  			match body with 
 			| Closure (w1, w2, w3) -> let env1 = (w1,eval enva e2)::enva in eval env1 w2
			(*	( 
				match w2 with
				Lambda ( a1, a2) -> Closure (a1, a2, env1) 
				| Add (a1, a2) -> Int 3
						 let env1 = ("y", Int 2)::env1 in eval env1 a1 + eval env1 a2
				| Sub (a1, a2) -> raise (Failure "incompatible type on App")
				| Div (a1, a2) -> raise (Failure "incompatible type on App")
				| Mul (a1, a2) -> raise (Failure "incompatible type on App")
				| Lt (a1, a2) -> raise (Failure "incompatible type on App")
				| Eq (a1, a2) -> raise (Failure "incompatible type on App")
				| And (a1, a2) -> raise (Failure "incompatible type on App")
				| If (a1, a2, a3) -> raise (Failure "incompatible type on App")
				| Let (a1, a2, a3) -> raise (Failure "incompatible type on App")
				| LetRec (a1, a2, a3) -> raise (Failure "incompatible type on App")
				| App (a1, a2) -> raise (Failure "incompatible type on App")
				| Id  a -> eval env1 e2  
				| Value a -> raise (Failure "incompatible type on App")				
				) *)
			| Int a -> body
			| Bool a -> raise (Failure "incompatible type on App")
			)
(*			|Int a  -> Failure "incompatible type on App")
  			|Bool a  -> Failure "incompatible type on App") 
*)  
  | Lambda (s1, e1) -> Closure(s1, e1, env)

let rec evaluate (e:expr) : value = eval [] e

let inc = Lambda ("n", Add(Id "n", Value (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )

(* The 'sumToN' function *)
let sumToN_expr : expr =
    LetRec ("sumToN", 
            Lambda ("n", 
                    If (Eq (Id "n", Value (Int 0)),
                        Value (Int 0),
                        Add (Id "n", 
                             App (Id "sumToN", 
                                  Sub (Id "n", Value (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(*
let twenty_one : value = evaluate (App (sumToN_expr, Value (Int 6)))
*)

(*
let x : int ref = ref 3 in
    let y : int = !x in
        (x := !x + 1);
        y + !x;;
*)
(* let rec app ((e1,e2):(expr,expr)) : value = 
	match (eval [] e1) with 
	add 
*)


(*let x : int ref = ref 3 in
    let y : int = !x+1 in
             y;;
 *)
(* 
let x : int ref = ref 3 in
    let y : int = !x+2 in
             y;;

*)
