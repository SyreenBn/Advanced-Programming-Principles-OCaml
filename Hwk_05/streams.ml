(* Stream examples *)

type 'a stream = Cons of 'a * (unit -> 'a stream)

let rec ones = Cons (1, fun () -> ones)

(* So, where are all the ones?  We do not evaluate "under a lambda", 
   which means that the "ones" on the right hand side is not evaluated. *)

(* Note, "unit" is the only value of the type "()".  This is not too
   much unlike "void" in C.  We use the unit type as the output type
   for functions that perform some action like printing but return no
   meaningful result.
   Here the lambda expressions doesn't use the input unit value,
   we just use this technique to delay evaluation of "ones". 
   Sometimes lambda expressions that take a unit value with the
   intention of delaying evaluation are called "thunks".  *)

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> tl ()

let rec from n = 
  Cons ( n, 
         fun () -> print_endline ("step " ^ string_of_int (n+1)) ; 
                   from (n+1) )

let nats = from 1

(* what is
    head nats,   head (tail nats),     head (tail (tail nats))     ?
 *)

let rec take (n:int) (s : 'a stream) : ('a list) =
 if n = 0 then []
 else match s with
      | Cons (v, tl) -> v :: take (n-1) (tl ())


(* Can we write functions like map and filter for streams? *)

let rec filter (f: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = (fun () -> filter f (tl ()))
     in
     if f hd then Cons (hd, rest) else rest ()

let even x = x mod 2 = 0

let rec squares_from n : int stream = Cons (n*n, fun () -> squares_from (n+1) )

let t1 = take 10 (squares_from 1)

let squares = squares_from 1


let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, fun () -> zip f (tl1 ()) (tl2 ()) )

let rec nats2 = Cons ( 1, fun () -> zip (+) ones nats2)

(* Computing factorials
   nats       = 1   2   3   4    5     6 
                 \   \   \   \    \     \
                  *   *   *   *    *     *
                   \   \   \   \    \     \
   factorials = 1-*-1-*-2-*-6-*-24-*-120-*-720
   We can define factorials recursively.  Each element in the stream
   is the product of then "next" natural number and the previous
   factorial.
 *)

let rec factorials = Cons ( 1, fun () -> zip ( * ) nats factorials )


(* The Sieve of Eratosthenes
   We can compute prime numbers by starting with the sequence of
   natual numbers beginning at 2. 
   We take the first element in the sequence save it as a prime number
   and then cross of all multiples of that number in the rest of the
   sequence.
   Then repeat for the next number in the sequence.
   This process will find all the prime numbers.
 *)

let non f x = not (f x)
let multiple_of a b = b mod a = 0

let sift (a:int) (s:int stream) = filter (non (multiple_of a)) s

let rec sieve s = match s with
  | Cons (x, xs) -> Cons(x, fun () -> sieve (sift x (xs ()) ))

let primes = sieve (from 2)


(* all work previece this comment is prof work, otherwise is my work *)


let rec cubes_from n = 
  Cons ( (n*n*n) , 
         fun () -> cubes_from (n+1) )

let check a b = b = a 

let drop_until (f:('a -> bool)) ( s :'a stream) : 'a stream  = filter f s

let rec drop (a : int) (x:'a stream) : 'a stream  =
     match x with
     | Cons (hd, tl) when (a = 0)->  Cons ( hd, fun () -> drop a ( tl() ) )
     | Cons (hd, tl) -> drop (a-1) (tl())

let rec map (f :('a -> 'b)) (s : 'a stream) : 'b stream =
  match s with
  | Cons (hd, tl) ->  Cons (f hd, fun () ->  map f (tl()) )

let squares_again = map (function x -> x*x) nats

let rec sqrt_approximations  (num:float) : ( float stream) = 
    let rec check (low, up) =
         if (((up -. low) < 0.001 ) || ((up -. low) = 0.001 ))
	 then Cons ( up, fun () ->  sqrt_approximations low )
         else let guess = ((low +. up) /. 2.0) in
	     if (guess *. guess > num)  then Cons ( guess, fun () ->  check ( low,guess ) )
             else Cons ( guess, fun () ->  check (guess,up))
    in check (1.0,num)

let rec from_dim n =
  Cons ( n,
         fun () -> from_dim (n /. 2.0) )

let diminishing = from_dim 16.00

let rec epsilon_diff (n : float) (s : float stream) : float = 
	match s with
	| Cons ( r, l) -> let hd = head (l()) in 
				if (abs_float(r -. hd) < n) then hd  
			  else epsilon_diff n (l())

let rough_guess = epsilon_diff 1.0 (sqrt_approximations 50.0) 

let precise_calculation = epsilon_diff 0.05 (sqrt_approximations 50.0) 

let rec sqrt_threshold (v:float) (t:float) : float =
    let ans = sqrt_approximations v in
    let rec help s t = 
    match s with 
    | Cons ( r, l) -> if ( (r *. r -. v) < t ) 
			then r
			else help ( l() ) t in help ans t  

