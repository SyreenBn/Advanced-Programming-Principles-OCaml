(* I work with Shawn. We discuss about [perimeter], [matrix_scalar_add] 
and [is_matrix]. I chose perimeter because it doesn’t return the right answer. 
It was returning 14. I asked him to check what is the wrong of it and he told me. 
Also, he was wondering how I built the function with one method and I explained 
the algorithm to him. We add the situation if the element of list is the last element 
and it works [  |(x,y)::[]->  (distance (1.0,1.0) (x,y)) ]. 
Also I realized that I use List.length in is_matrix but I should not do that. So we 
discuss about how to build your length list and I rebuilt it again correctly. The last thing 
we discuss about matrix_scalar_add. He asked me what is the wrong of his function. 
It used to have some mistake in the order of call method command. Also, I tried to 
make perimeter more general but I could not. *)

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

let distance (x,y) ( z,t)= sqrt(((z-.x)**2.0 +. (t-.y)**2.0))

let rec perimeter (lis : (float*float) list): float  =
match lis with
|[]-> 0.0
|(x,y)::[]->  (distance (1.0,1.0) (x,y))
|(x,y)::(( z,t)::rest) -> (distance (x,y) ( z,t) ) +. (perimeter ((z,t)::rest))

let rec add lis i =
match lis with
|[]->[]
|x1:: rest -> (x1+i):: add rest i

let rec matrix_scalar_add x i =
       match x with
      | []-> []
      | x1::(rest)-> add x1 i ::  matrix_scalar_add rest i

let rec length list = 
match list with
| []-> 0
| x1::[]-> 1
| x1::rest -> 1+ (length rest)

let rec is_matrix x =
match x with
| []-> true
| x1::[]->true
| x1::(x2::rest) -> (length (x1) = length (x2)) && ( is_matrix  (x2 :: rest))
