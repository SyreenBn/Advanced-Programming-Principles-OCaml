type formula = And of formula * formula
	     | Or  of formula * formula
	     | Not of formula 
	     | Prop of string
	     | True
	     | False


exception KeepLooking

type subst = (string * bool) list

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair

let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []

let rec lookup (n:string) (env: subst) =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " not in scope"))
  | (n',v) :: rest when n = n' -> v
  | _ :: rest -> lookup n rest

let rec freevars (e:formula)  : string list =
  match e with
  | True -> []
  | False -> []
  | Or (e1, e2) -> dedup (freevars e1 @ freevars e2)
  | And (e1, e2) -> dedup (freevars e1 @ freevars e2)
  | Not (e1) -> dedup (freevars e1)
  | Prop i -> [i] 

let rec eval (f:formula) (sb:subst) : bool =
  match f with 
  | True -> true
  | False -> false
  | And (f1, f2) -> eval f1 sb &&  eval f2 sb 
  | Or (f1, f2) -> eval f1 sb ||  eval f2 sb
  | Not f1 -> not (eval f1 sb)
  | Prop f1 -> lookup f1 sb

let rec tf_lst lst =
match lst with
|[] -> []
|x::xs -> (x, false) :: (x, true) :: tf_lst xs

let gen_subsets lst
= let rec helper partial_subset rest
  = match rest with
  | [] -> [ partial_subset ]
  | x::xs -> (helper ((x,true) :: partial_subset)  xs)
               @
             (helper ((x,false) :: partial_subset)  xs)
  in helper [] lst

let rec drop x =
match x with
[] -> []
| ((a,b) :: (c,d) ::[] ) :: xs when a = c ->  drop xs
| ((a,b) :: (c,d) ::[] ) :: xs ->  ((a,b) :: (c,d) :: [] ):: drop xs
| ((a,b)::[]) :: xs ->  ((a,b) :: []) :: drop xs
| _ :: _ -> drop [];;


let head lst =
match lst with
[] -> []
|x::xs -> x

let rec double_to_one lst =
match lst with
|[[]]-> []
|[x]::xs -> x:: double_to_one xs
|_ -> []

let rec process_solution_exn show s =
  print_endline ( "Here is a solution:\n" ^ show s) ;
  print_endline ("Do you like it?") ;

  match is_elem 'Y' (explode (String.capitalize (read_line ()))) with
  | true  -> print_endline "Thanks for playing..." ; s
  | false -> raise KeepLooking

let extract s = match s with
|[]-> raise (Failure "the list is empty")
|x :: xs -> x 

let is_tautology (f : formula ) (fn : (subst -> subst option)) =
  let lst = gen_subsets ( freevars (f) ) in
  let part_lst = List.filter (fun x -> eval f x = false) lst in
   let ans = List.filter (fun x -> x <> [])  part_lst in
  let rec helper sol rest =
     if ( sol <> [] && rest = [])
   	then  fn (extract sol) 
   	else
     	(match rest with 
		|[] -> raise KeepLooking
		|x::xs -> (try helper (sol @ [x]) xs with
                     | KeepLooking -> helper sol  xs)) 
     in try helper [] ans with 
	  | KeepLooking -> None



(*
let is_tautology (f : formula ) (fn : (subst -> subst option)) : subst option =
  let lst = gen_subsets ( freevars (f) ) in
  let part_lst = List.filter (fun x -> eval f x = false) lst in
  let ans = List.filter (fun x -> x <> [])  part_lst in
         fn (  (List.map ( fun x -> process_solution_exn show_string_bool_pair (extract x)) ans)
                 )
*)
let is_tautology_first f = is_tautology f (fun s -> Some s)

let is_tautology_print_all f =
  is_tautology 
    f
 
   (fun s -> print_endline (show_subst s); 
	      raise KeepLooking)

