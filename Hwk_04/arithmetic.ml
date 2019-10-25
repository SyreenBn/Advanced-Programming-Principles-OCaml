type expr 
  = Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr

let rec show_expr (b:expr): string =
  match b with
  | Const x -> Printf.sprintf "%d" x
  | Add (e1, e2) -> "(" ^ show_expr e1 ^ "+" ^ show_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ show_expr e1 ^ "-" ^ show_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ show_expr e1 ^ "*" ^ show_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ show_expr e1 ^ "/" ^ show_expr e2 ^ ")"

let rec show_expr1 (b:expr): string =
  match b with
  | Const x -> Printf.sprintf "%d" x
  | Add (e1, e2) -> "(" ^ show_expr1 e1 ^ "+" ^ show_expr1 e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ show_expr1 e1 ^ "-" ^ show_expr1 e2 ^ ")"
  | Mul (e1, e2) ->  show_expr1 e1 ^ "*" ^ show_expr1 e2
  | Div (e1, e2) ->  show_expr1 e1 ^ "/" ^ show_expr1 e2

let rec show_pretty_expr (b:expr): string =
  match b with
  | Const x -> Printf.sprintf "%d" x
  | Add (e1, e2) -> show_pretty_expr e1 ^ "+" ^ show_expr1 e2
  | Sub (e1, e2) -> show_pretty_expr e1 ^ "-" ^ show_expr1 e2
  | Mul (e1, e2) -> show_pretty_expr e1 ^ "*" ^ show_expr1 e2
  | Div (e1, e2) -> show_pretty_expr e1 ^ "/" ^ show_expr1 e2


