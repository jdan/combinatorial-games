let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let rec all pred = function
  | [] -> true
  | x :: rest -> if pred x then all pred rest else false

let rec any pred = function
  | [] -> false
  | x :: rest -> if pred x then true else any pred rest

let rec update idx v = function
  | [] -> []
  | x::xs -> if idx = 0 then (v :: xs) else x :: update (idx - 1) v xs
