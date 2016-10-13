type constantc_type =
  | Int
  | Bool
  | Null

let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Null -> "Null"
