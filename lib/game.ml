let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let rec all pred = function
  | [] -> true
  | x :: rest -> if pred x then all pred rest else false

(* unused *)
(* let rec any pred = function
   | [] -> true
   | x :: rest -> if pred x then true else any pred rest *)

let rec update idx v = function
  | [] -> []
  | x::xs -> if idx = 0 then (v :: xs) else x :: update (idx - 1) v xs

module type CombinatorialGame = sig
  type result
  type game_state

  val next_states : game_state -> game_state list

  (* TODO: Functor-ize *)
  val eval : game_state -> result
end

module SinglePileNim : CombinatorialGame = struct
  type result = Win | Lose
  type game_state = int

  let next_states pile = range 0 (pile - 1)

  (* todo: memoize *)
  let rec eval = function
    | 0 -> Lose
    | state ->
      let eval_next = next_states state |> List.map eval in
      (* SHAPE: Page 112 *)
      (* If every move I make leads to a W, my current position is an L. *)
      if all (fun r -> r = Win) eval_next
      then Lose
      (* If some move I can make leads to an L, my current position is a W. *)
      else Win

  let%test _ = Win = eval 5
  let%test _ = Win = eval 4
  let%test _ = Win = eval 1
  let%test _ = Lose = eval 0
end

module MultiPileNim : CombinatorialGame = struct
  type result = Win | Lose
  type game_state = int list

  let next_states piles =
    List.mapi
      (fun idx pile ->
         range 0 (pile - 1)
         |> List.map (fun new_pile -> update idx new_pile piles)
      )
      piles
    |> List.concat

  (* todo: memoize *)
  let rec eval state =
    if all (fun n -> n = 0) state
    then Lose
    else
      let eval_next = next_states state |> List.map eval in
      (* SHAPE: Page 112 *)
      (* If every move I make leads to a W, my current position is an L. *)
      if all (fun r -> r = Win) eval_next
      then Lose
      (* If some move I can make leads to an L, my current position is a W. *)
      else Win

  let%test _ = Win = eval [5]
  let%test _ = Lose = eval [2; 2]
  let%test _ = Win = eval [2; 3]
end
