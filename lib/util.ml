let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let rec all pred = function
  | [] -> true
  | x :: rest -> if pred x then all pred rest else false

let rec any pred = function
  | [] -> false
  | x :: rest -> if pred x then true else any pred rest

let rec first pred = function
  | [] -> None
  | x :: rest ->
    if pred x
    then Some x
    else first pred rest

let firsti pred ls =
  let rec inner idx pred = function
    | [] -> None
    | x :: rest ->
      if pred x
      then Some idx
      else inner (idx + 1) pred rest
  in inner 0 pred ls

let find_alli pred ls =
  let rec inner idx pred = function
    | [] -> []
    | x :: rest ->
      if pred x
      then idx :: inner (idx + 1) pred rest
      else inner (idx + 1) pred rest
  in inner 0 pred ls

let rec update idx v = function
  | [] -> []
  | x::xs -> if idx = 0 then (v :: xs) else x :: update (idx - 1) v xs

let (||*) a b =
  if Option.is_some a
  then a
  else b
