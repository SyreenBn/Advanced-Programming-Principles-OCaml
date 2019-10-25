type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree

(*--------------------------------------------------------------------------*)

let rec t_size t =
  match t with
  | Leaf _ -> 1
  | Fork (_, ta, tb) -> 1 + t_size ta + t_size tb

let rec t_sum t =
  match t with
  | Leaf x -> x
  | Fork (xs, ta, tb) -> xs + t_sum ta + t_sum tb


let rec t_charcount t =
  match t with
  | Leaf x -> String.length x
  | Fork (xs, ta, tb) -> String.length xs + t_charcount ta + t_charcount tb

let rec t_concat t =
  match t with
  | Leaf x -> x
  | Fork (xs, ta, tb) -> xs ^ t_concat ta ^ t_concat tb

(*--------------------------------------------------------------------------*)

let rec t_opt_size (t : 'a option tree) : int =
    match t with
        | Leaf x ->
                (match x with
                None -> 0
                |Some _ -> 1)
        | Fork (xs, ta, tb) ->  (match xs with
                                None -> 0
                                |Some _ -> 1) + t_opt_size ta + t_opt_size tb

let rec t_opt_sum (t : 'a option tree) : int =
    match t with
        | Leaf x ->
                ( match x with
                None -> 0
                |Some y -> y )
        | Fork (xs, ta, tb) ->  (match xs with
                                None -> 0
                                |Some ys -> ys) + t_opt_sum ta + t_opt_sum tb

let rec t_opt_charcount (t : 'a option tree) : int =
    match t with
        | Leaf x ->
                ( match x with
                None -> 0
                |Some y -> String.length y )
        | Fork (xs, ta, tb) ->  (match xs with
                                None -> 0
                                |Some ys -> String.length ys)+ t_opt_charcount ta  + t_opt_charcount tb

let rec t_opt_concat t  =
    match t with
        | Leaf x ->
                ( match x with
                None -> ""
                |Some y -> y )
        | Fork (xs, ta, tb) ->  (match xs with
                                None -> ""
                                |Some ys -> ys) ^  t_opt_concat ta ^ t_opt_concat tb

(*--------------------------------------------------------------------------*)
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree

let rec tfold (l:'a -> 'b)(f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b =
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size t = tfold (fun x -> 1)(fun a b c -> 1 + b + c) t

let tf_sum t = tfold (fun x -> x)(fun a b c -> a+b+c) t

let tf_char_count t = tfold (fun x -> String.length x)
				(fun a b c -> (String.length a) + b + c ) t

let tf_concat t = tfold (fun x -> x)(fun a b c -> (a ^ b ^ c )) t

let tf_opt_size t = tfold ( fun x -> match x with
					None -> 0
					|Some _ -> 1) ( fun a b c -> (match a with
                                        				None -> 0
                                        				|Some _ -> 1 )+ b + c ) t

let tf_opt_sum t = tfold ( fun x -> match x with
                                        None -> 0
                                        |Some ys -> ys) ( fun a b c -> (match a with
                                                                        None -> 0
                                                                        |Some xs -> xs )+ b + c ) t

let tf_opt_char_count t = tfold ( fun x -> match x with
                                        None -> 0
                                        |Some ys -> String.length ys) ( fun a b c -> (match a with
                                                                        None -> 0
                                                                        |Some xs -> String.length xs)+ b + c ) t

let tf_opt_concat t = tfold ( fun x -> match x with
                                        None -> ""
                                        |Some ys -> ys) ( fun a b c -> (match a with
                                                                        None -> ""
                                                                        |Some xs -> xs)^ b ^ c ) t

(*--------------------------------------------------------------------------*)
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree
(*--------------------------------------------------------------------------*)

let rec compare x = function
		| Empty -> Node (Empty, x, Empty)
		| Node (l, y, r) ->
		if x = y then Node (l, x ,r)
		else if x < y then Node (compare x l, y, r)
		else Node (l, y, compare x r)

let rec bt_insert_by f x t = compare x t 

let rec bt_elem_by f x t =
        match t with
          Empty -> false
         |Node ( left , y , right) -> if( f y x ) then true
					else  bt_elem_by f x left || bt_elem_by f x right  
 
let rec bt_to_list t = 
		  match t with  
                  Empty -> []
                  |Node (left, y, right) -> bt_to_list left @ (y :: bt_to_list right) 

let rec btfold (a:'b)(f:'b -> 'a -> 'b -> 'b)(t:'a btree):'b = 
	match t with
	Empty ->  a
	| Node (left, y , right) -> (f y) (btfold a f left) (btfold a f right) 
(*
let btf_elem_by f a t = btfold [] (fun x y z -> (if compare( (=) a y) then y::(x@z) else  ) ) t
*)


(* it is diffeculte to write btf_insert_by by tbfold because it is diffecult
 to determine the right place of the node to make it balance tree. in the recursionss
we can compare the new node  with the element of the node and then pass to left tree 
if the new node less than or to the right tree if the new node bigger than the current node *) 

let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))
let t7 = (Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None)))

