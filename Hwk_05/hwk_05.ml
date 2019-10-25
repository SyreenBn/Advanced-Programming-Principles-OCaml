let rec ands (l : bool list) : bool = 
match l with
  |v :: vx when (v = false) -> false
  |v :: vx -> v && ands vx
  | [] -> true

let rec foldr f l v = match l with
| [] -> v
| x::xs -> f x (foldr f xs v)

let and_f b1 b2 = if b1 then b2 else false

let and_r l = foldr and_f l true

