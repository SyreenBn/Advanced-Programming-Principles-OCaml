

module type Comparable = sig
	type t
	val add : t -> t -> t 
	val mul : t -> t -> t 
	val string_of : t -> string
	val zero : t
   end

module type Arithmetic_intf = sig
	type t
	type entries
	val create : int -> entries -> t
	val from_list : entries list -> t
	val to_list : t -> entries list
	val scalar_add : entries -> t -> t
	val scalar_mul : entries -> t -> t
	val scalar_prod : t -> t -> entries option
	val print_list :  entries list -> string
	val to_string : t -> string
	val size : t -> int
   end
	
module Make_vector ( Entries: Comparable) : (Arithmetic_intf with type entries := Entries.t) = struct 

type entries = Entries.t  

type t = |Vector of int * Entries.t list
   	           | Empty

let create m n =
   if m = 0 then Empty
   else 
   (let rec helper a b =
       if a = 0 then []
                else b :: helper (a-1) b 
    in Vector (m, helper m n))
    
let from_list lst = 
if List.length lst = 0 then Empty
		       else Vector ( List.length lst, lst)

let to_list v  = 
match v with 
|Empty -> []
|Vector (a,b) -> b

let scalar_add n v   =
match v with
|Empty -> Empty
|Vector (a,b) -> Vector ( a, List.map(fun x -> Entries.add x n) b )

let scalar_mul n v  =
match v with
|Empty -> Empty
| Vector (a,b) -> Vector ( a, List.map(fun x -> Entries.mul x n) b)

let scalar_prod v1 v2 = 
match v1, v2 with
|Empty,_ -> None
|_, Empty -> None
|(Vector(a,b), Vector(c,d)) -> if a = c 
				then Some (let rec helper lst1 lst2 =
					match lst1, lst2 with
					|([],_) -> Entries.zero
					|(_,[]) -> Entries.zero
					|(x1::xs1,x2::xs2) -> Entries.add (Entries.mul x1 x2) (helper xs1 xs2) 
				     in  helper b d )  
				else None

let rec print_list lst =
match lst with
|[] -> ""
|x::xs when (List.length xs >= 1) -> (Entries.string_of x)^ ", " ^ print_list xs
|x::xs -> (Entries.string_of x) ^ " " ^ print_list xs

let to_string v =
match v with 
|Empty -> ""
|Vector (a,b) -> "<< " ^ (string_of_int a) ^ " | " ^ (print_list b) ^ ">>"

let size v =
match v with
|Empty -> 0
|Vector (a,b) -> a

end




module Int_arithmetic : (Comparable with type t = int) = struct
     type t = int
        let add n b = n + b
        let mul n b = n * b
        let string_of a = string_of_int a
        let zero = 0
end

module Int_vector = Make_vector (Int_arithmetic)

let v1 = Int_vector.create 10 1

let v2 = Int_vector.from_list [1;2;3;4;5]

let v3 = Int_vector.scalar_add 3 v2

let v4 = Int_vector.scalar_mul 10 v2

let i1 = Int_vector.scalar_prod v3 v4

let l1 = Int_vector.to_list v3

let i2 = Int_vector.size v4

let s1 = Int_vector.to_string v1

let s2 = Int_vector.to_string v2

let s3 = Int_vector.to_string v3

let s4 = Int_vector.to_string v4

module Complex_arithmetic : (Comparable with type t = (float*float)) = struct
     type t = float*float
        let add (a,b) (c,d) = ((a +. c),(b +. d))
        let mul (a,b) (c,d) = (((a *. c) -. (b *. d)) , ( (a *. d) +. (b *. c)))
        let string_of (a,b) = "(" ^ string_of_float a ^ "+" ^ string_of_float b ^ "i" ^ ")"
        let zero = (0.0,0.0)
end




module Complex_vector = Make_vector (Complex_arithmetic)

let v5 = Complex_vector.from_list [ (1.0, 2.0); (3.0, 4.0); (5.0, 6.0) ]

let v6 = Complex_vector.scalar_add (5.0, 5.0) v5

let c1 = Complex_vector.scalar_prod v5 v6

let s5 = Complex_vector.to_string v5

let s6 = Complex_vector.to_string v6


