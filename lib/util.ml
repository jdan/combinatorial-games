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

(* https://dev.realworldocaml.org/imperative-programming.html *)
let memoize f =
  let memo_table = Hashtbl.create 24 in
  fun x ->
    match Hashtbl.find_opt memo_table x with
    | None ->
      let result = f x
      in
      Hashtbl.add memo_table x result;
      result
    | Some v ->
      v

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f x
