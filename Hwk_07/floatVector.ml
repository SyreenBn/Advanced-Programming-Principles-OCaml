open Hwk_07

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
