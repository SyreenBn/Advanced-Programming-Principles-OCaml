let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []
(* val take : int -> 'a list -> 'a list *)

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l
(* val drop : int -> 'a list -> 'a list *)

let find_all_by equals e l =
  	List.fold_right (fun x a -> if equals x e then x :: a else a) l []
(* val find_all_by : ('a -> 'b -> bool) -> 'b -> 'a list -> 'a list *)

let is_elem_by equals e l=
match find_all_by equals e l with
| [] -> false
| _::_  -> true
(* val is_elem_by : ('a -> 'b -> bool) -> 'b -> 'a list -> bool*)

let check s2 s1 = s1 = s2
(*val check : 'a -> 'a -> bool*)

let is_elem e lst = is_elem_by check e lst
(* val is_elem : 'a -> 'a list -> bool  *)

let length lst = List.fold_left (fun v _ -> v + 1) 0 lst
(* val length : 'a list -> int *)

let rev lst = List.fold_left (fun a x -> x :: a) [] lst
(* val rev : 'a list -> 'a list *)

let del xs x = if List.mem x xs then xs else x :: xs 
(* val del : 'a list -> 'a -> 'a list *)

let dedup xs = List.rev (List.fold_left del [] xs )
(* val dedup : 'a list -> 'a list *)

type word = char list

type line = word list

type result = OK 
        | FileNotFound of string
        | IncorrectNumLines of int 
        | IncorrectLines of (int * int) list
        | IncorrectLastStanza

type charListOption =  String of string
(*type charListOption =  bytes*)

let split_by equals list ls =
let (x,y) =  List.fold_right (fun x (a,b) -> if is_elem_by equals x ls 
						then ([],a::b) 
						else (x::a,b)  ) list ([],[])
						in x::y
(* val split_by : ('a -> 'b -> bool) -> 'b list -> 'a list -> 'b list list *)

let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

let pun = [' ';'.' ; '!' ; '?' ; ',' ; ';' ; ':' ; '-']

let find_line list = split_by (=) list ['\n']
(* val find_line : char list -> char list list *)

let convert_to_non_blank_lines_of_words list =
	let lst = (List.map (fun y -> split_by (=) y pun )(find_line list) )in 
		(List.filter(fun x -> x != []) ( List.map(fun c -> 
						( find_all_by (!=) [] c)) lst))
(* val convert_to_non_blank_lines_of_words : char list -> char list list list *)

let paradelle ( fileName : string ) : result =
	let file = (read_file fileName)in
	match file with
	 None -> FileNotFound fileName 	
	|Some tx -> (let convText = convert_to_non_blank_lines_of_words (tx) in 
		let line1 = take 1 convText in
                let line2 = take 2 convText in 
		let line11 = take 11 convText in
                let line12 = take 12 convText in 
                let line17 = take 17 convText in
                let line18 = take 18 convText in
		match convText with
		|tx when (length convText != 24) -> IncorrectNumLines (length convText) 
  		|tx when (line1 != line2 && line11 != line12 && line17 = line18 ) -> IncorrectLines [(1, 2); (11, 12); (17, 18)]
	(*	|tx when (line11 != line12 && line17 != line18 ) -> IncorrectLines [(11, 12); (17, 18)]*)
		|tx -> OK)

