

open Hwk_07

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


 
