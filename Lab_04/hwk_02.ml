let find_all_by equals e l =
  	List.fold_right (fun x a -> if equals e x then x :: a else a) l []

let is_elem_by equals e l=
match find_all_by equals e l with
| [] -> false
| _::_  -> true

let check s2 s1 = s1 = s2

let is_elem e lst = is_elem_by check e lst;

let length lst = List.fold_left (fun v _ -> v + 1) 0 lst

let rev lst = List.fold_left (fun a x -> x :: a) [] lst
