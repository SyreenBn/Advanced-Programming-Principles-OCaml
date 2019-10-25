let even x =
        if (x mod 2)=0 then true
        else false

 let rec euclid x y =
        if (x = y) then x
        else if (x < y) then euclid x(y-x)
        else euclid(x-y) y

let frac_add (n1,d1) (n2,d2) =
              ((d2*n1)+(n2*d1),(d1*d2))

let rec frac_simplify (x,y)= 
       if ( (y mod x != 0) || (x =1) ) then (x,y)
        else frac_simplify ((x/(y/x)), (y/(y/x)))

let square_approx num acc = 
    let rec check (low, up) =
         if (((up -. low) < acc) || ((up -. low) = acc ))
then (low,up)
        else let guess = ((low +. up) /. 2.0) in
if (guess*.guess > num)  then check (low,guess )
        else check (guess,up)
    in check (1.0,num)

let rec max_list myList = match myList with
| [] -> 0
| a::[] -> a
| a :: (b::rest) -> if  (a > b || a = b) then max a  (max_list (b::rest)) else max b (max_list (b::rest))

let rec drop i l =
     match l with
     | [] -> []
     | x::b when i = 0 -> x::b
     | h::t -> drop(i-1) t

let rec rev = function
  [] -> [] 
 | element :: list -> (rev list) @ [element]

let rec add lis i =
match lis with
|[]->[]
|x1:: rest -> (x1+i):: add rest i

let rec matrix_scalar_add x i =
       match x with
      | []-> []
      | x1::(rest)-> add x1 i ::  matrix_scalar_add rest i

let rec perimeter (lis : (float*float) list): float  =
match lis with
|[]-> 0.0
|(_ , _)::[]->0.0
|(x,y)::(( z,t)::rest) when (List.length rest = 0) ->  (distance (1.0,1.0) (x,y))
|(x,y)::(( z,t)::rest) -> (distance (x,y) ( z,t) ) +. (perimeter ((z,t)::rest))

let rec is_matrix x =
match x with
| []-> true
| x1::[]->true
| x1::(x2::rest) -> (List.length x1 = List.length x2) && ( is_matrix  (x2 :: rest))


