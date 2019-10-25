let circle_area_v1 x = 3.1415 *. (x/.2.0) *. (x/.2.0)

let circle_area_v2 x =
        let pi = 3.1415 in pi *. (x/.2.0) *. (x/.2.0)

let rec product xs =
  match xs with
  | [ ] -> 1
  | x::rest -> x * product rest

let rec sum_diffs x =
       match x with 
      | x1::(x2::[]) -> x1 - x2
      | x1::(x2::rest) -> (x1 - x2) + (sum_diffs (x2 :: rest))

let distance (x,y) ( z,t)= sqrt(((z-.x)**2.0 +. (t-.y)**2.0))

let triangle_perimeter (x,y) (z,t) (a,b)= 
sqrt(((z-.x)**2.0 +. (t-.y)**2.0)) +. 
sqrt(((z-.a)**2.0 +. (t-.b)**2.0)) +. 
sqrt(((a-.x)**2.0 +. (b-.y)**2.0))
