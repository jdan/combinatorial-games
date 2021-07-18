type result = Win | Lose

(* dunno if I want `turn` *)
type turn = P1 | P2

type game_state =
  { turn : turn
  ; pile : int
  }

let rec all pred = function
  | [] -> true
  | x :: rest -> if pred x then all pred rest else false

(* unused *)
let rec any pred = function
  | [] -> true
  | x :: rest -> if pred x then true else any pred rest

let rec range a b =
  if a > b then []
  else a :: range (a + 1) b

let next_states = function
  | { pile = pile ; turn = turn } ->
    range 0 (pile - 1)
    |> List.map (
      fun n -> { pile = n
               ; turn = if turn = P1 then P2 else P1
               }
    )

(* todo: memoize *)
let rec eval = function
  | { pile = 0 ; _ } -> Lose
  | state ->
    let eval_next = next_states state |> List.map eval in
    (* SHAPE: Page 112 *)
    (* If every move I make leads to a W, my current position is an L. *)
    if all (fun r -> r = Win) eval_next
    then Lose
    (* If some move I can make leads to an L, my current position is a W. *)
    else Win

let%test _ = Win = eval { pile = 5; turn = P1 }
let%test _ = Win = eval { pile = 4; turn = P1 }
let%test _ = Win = eval { pile = 1; turn = P1 }

let%test _ = Lose = eval { pile = 0; turn = P1 }
let%test _ = Win = eval { pile = 5; turn = P2 }
