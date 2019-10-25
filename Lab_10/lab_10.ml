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







(*Run on March 31, 14:48:09 PM. this is the firt time of submitting the lab work, but I remeber that I did not refer to where prof work is and where is my work. 
All fun berfore this comment is proff work, other wise it is my work *) 


let rec str_from (n : int) : string stream =
  Cons (  string_of_int(n) ,
         fun () -> print_endline ("step " ^ string_of_int (n+1)) ;
                   str_from (n+1) )

let rec separators (n : int) (s : string) : string stream =
     let rec helper (count, str) =
         if (count = 0)
                then  Cons ( "\n" ,
         fun () -> print_endline ("step " ^ string_of_int (count+1));
                                  helper (n, s ) )
                else  Cons ( s ,
         fun () -> print_endline ("step " ^ string_of_int (count+1));
                                  helper ((count-1), str) )in helper (n,s)

let str_nats = str_from 1
(*
let rec alternate (s : 'a stream) ( n : 'a stream) : 'a stream =
        let count = 0 in 
        let rec helper (sst ,nst ,count)=
        match sst, nst with
        |Cons ( a, b), Cons ( d, e) when (count mod 2 = 0) -> Cons ( a ,fun () -> print_endline ("step " ^ string_of_int (count+1));
                                                                  helper (Cons ( (a+1), b), Cons ( (d), e), count-1 ) )
        |Cons ( a, b), Cons ( d, e) ->                        Cons ( d ,fun () -> print_endline ("step " ^ string_of_int (count+1));
                                                                   helper (Cons ( (a), b), Cons ( (d+1), e), count-1 ) ) in helper (s, n, count)

let str_105_nats = take 105 str_nats

let str_105_sep = let n = 0 in List.map ( fun x -> if (n mod 10 = 0) then take 1 (separators (n+1) x) else take 1 (separators (n+1) x) ) str_105_nats 

let str_105_nats = take 105 ( alternate str_nats (separators 10 "\n"))

*)
let rec alternate  (s : 'a stream) ( n : 'a stream) : 'a stream =
        match s, n with
        |Cons ( a, b), Cons ( d, e) -> Cons ( a, fun () -> Cons ( d, fun () -> alternate (b()) (e()) ) )

let ten_per_line = separators 9 ", "

let str_105_nat = take 209 ( alternate str_nats ten_per_line )

let str_105_nats = List.fold_right (fun a b ->  a ^ b ) str_105_nat ""

