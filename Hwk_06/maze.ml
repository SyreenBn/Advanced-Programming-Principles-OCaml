let rec is_not_elem set v =
  match set with
  | [] -> true
  | s::ss -> if s = v then false else is_not_elem ss v

let run e = (fun x -> ()) e

let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let rec implode = function
  | []    -> ""
  | c::cs -> String.make 1 c ^ implode cs

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

type loc = S | G
let final s = s = (3,5) || s = (5,1)

type state = int * int 
(*
let ok_state ( (a,b) : state) : bool  =  
(a = 3 && b = 5) || (a = 5 && b = 1)
*)

let walls = []

let run e = (fun x -> ()) e

exception KeepLooking

let rec process_solution_exn show s = 
  print_endline ( "Here is a solution:\n" ^ show s) ;
  print_endline ("Do you like it?") ;

  match is_elem 'Y' (explode (String.capitalize (read_line ()))) with
  | true  -> print_endline "Thanks for playing..." ; Some s
  | false -> raise KeepLooking

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_loc = function
  | S -> "S"
  | G -> "G"

let show_state (a,b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
(*
let show_path = show_list show_state

let maze () =
  let rec go_from state path =
    if final state 
    then process_solution_exn show_path path 
    else
*)


let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []


let move_up (a,b) = (a-1,b)
let move_down (a,b) = (a+1,b)
let move_right (a,b) = (a,b+1)
let move_left (a,b) = (a,b-1)

 let rec tail lst=
match lst with
|[]-> raise (Failure "wrong")
|a :: [] -> a
|a::b -> tail b

let walls = [
[(1,1);(1,2)];
[(2,1);(2,2)];
[(2,2);(2,3)];
[(3,1);(4,1)];
[(4,1);(5,1)];
[(4,2);(5,2)];
[(4,2);(4,3)];
[(4,3);(4,4)];
[(3,4);(3,5)];
[(2,5);(3,5)];
[(4,4);(5,4)];
[(5,4);(5,5)];
[(2,3);(3,3)];
[(2,3);(2,4)];
[(1,4);(2,4)];

[(1,1);(0,1)];
[(1,2);(0,2)];
[(1,3);(0,3)];
[(1,4);(0,4)];
[(1,5);(0,5)];

[(1,1);(1,0)];
[(2,1);(2,0)];
[(3,1);(3,0)];
[(4,1);(4,0)];
[(5,1);(5,0)];

[(5,1);(6,1)];
[(5,2);(6,2)];
[(5,3);(6,3)];
[(5,4);(6,4)];
[(5,5);(6,5)];

[(1,5);(1,6)];
[(2,5);(2,6)];
[(3,5);(3,6)];
[(4,5);(4,6)];
[(5,5);(5,6)];]

let show_path = show_list show_state



let maze_moves  (lst:int * int) : (int * int) list =   
	let up (x,y) = if (is_not_elem walls [(x,y);(x-1,y)] && is_not_elem walls [(x-1,y);(x,y)]) 
			then  [(x-1,y)]
			else [] in  
	let down (x,y) = if (is_not_elem walls [(x,y);(x+1,y)] && is_not_elem walls [(x+1,y);(x,y)]) 
			then [(x+1,y)]
			else [] in
	let left (x,y) = if (is_not_elem walls [(x,y);(x,y-1)] && is_not_elem walls [(x,y-1);(x,y)])
                        then [(x,y-1)]
                        else [] in
	let right (x,y) = if (is_not_elem walls [(x,y);(x,y+1)] && is_not_elem walls [(x,y+1);(x,y)]) 
			then [(x,y+1)]
			else []
	in  (up lst @ down lst @ right lst @ left lst )


let maze(): (int * int) list option =
  let rec go_from state (path:(int * int) list) =
    if final state
    then process_solution_exn show_path path (*  Some path*)
    else
      match List.filter (is_not_elem path) (maze_moves state) with
      | [] -> raise KeepLooking
      | [a] -> (go_from a (path @ [a]) )
      | [a;b] -> (try go_from a (path @ [a]) with | KeepLooking -> go_from b (path @ [b]))
	
      | [a;b;c] -> (try
			(try go_from a (path @ [a]) 
		         with | KeepLooking -> go_from b (path @ [b])
			) 
		   with | KeepLooking -> (go_from b (path @ [c])) 
		   )
      
      | [a;b;c;d] -> (try 
		   (try
                        (try go_from a (path @ [a])
                         with | KeepLooking -> go_from b (path @ [b])
                        )
                   with | KeepLooking -> (go_from b (path @ [c]))
                   ) with | KeepLooking -> (go_from b (path @ [c])) )

      | _ -> raise (Failure ("No way to move 3 things!"))
  in try go_from (2,3) [ (2,3) ] with 
	| KeepLooking -> None
