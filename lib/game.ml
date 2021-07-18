open Util

type result = Win | Lose | Draw

module type CombinatorialGame = sig
  type game_state

  val next_states : game_state -> game_state list
  val immediate_result : game_state -> result option
end

module CombinatorialGameEvaluator (G : CombinatorialGame) = struct
  open G

  (* todo: memoize *)
  let rec eval state =
    match immediate_result state with
    | Some result -> result
    | None ->
      let eval_next = next_states state |> List.map eval in
      (* SHAPE: Page 126 *)
      (* If every move I make leads to a W, my current position is an L. *)
      if all (fun r -> r = Win) eval_next
      then Lose
      (* If some move I can make leads to an L, my current position is a W. *)
      else if any (fun r -> r = Lose) eval_next
      then Win
      else Draw
end

module SinglePileNim = struct
  type game_state = int

  let next_states pile = range 0 (pile - 1)
  let immediate_result state = if state = 0 then Some Lose else None
end

module SinglePileNimEvaluator = CombinatorialGameEvaluator(SinglePileNim)
let%test _ = Win = SinglePileNimEvaluator.eval 5
let%test _ = Win = SinglePileNimEvaluator.eval 4
let%test _ = Win = SinglePileNimEvaluator.eval 1
let%test _ = Lose = SinglePileNimEvaluator.eval 0

module MultiPileNim = struct
  type game_state = int list

  let next_states piles =
    List.mapi
      (fun idx pile ->
         range 0 (pile - 1)
         |> List.map (fun new_pile -> update idx new_pile piles)
      )
      piles
    |> List.concat
  let%test _ = [[0; 0]; [0; 1]] = next_states [0; 2]
  let%test _ = [[0; 2]; [1; 2]; [2; 0]; [2; 1]] = next_states [2; 2]

  let is_loss = all (fun n -> n = 0)
  let immediate_result piles =
    if all (fun n -> n = 0) piles
    then Some Lose
    else None
end

module MultiPileNimEvaluator = CombinatorialGameEvaluator(MultiPileNim)
let%test _ = Win = MultiPileNimEvaluator.eval [5]
let%test _ = Lose = MultiPileNimEvaluator.eval [2; 2]
let%test _ = Win = MultiPileNimEvaluator.eval [2; 3]

module FlagPick = struct
  type game_state = int
  (* You can pick 1, 2, or 3 flags *)
  let next_states n =
    if n < 4
    then range 0 (n - 1)
    else [n - 1; n - 2; n - 3]
  let immediate_result state = if state = 0 then Some Lose else None
end

module FlagPickEvaluator = CombinatorialGameEvaluator(FlagPick)
let%test _ = Win = FlagPickEvaluator.eval 5
let%test _ = Win = FlagPickEvaluator.eval 3
let%test _ = Win = FlagPickEvaluator.eval 2
let%test _ = Win = FlagPickEvaluator.eval 1

let%test _ = Lose = FlagPickEvaluator.eval 0
let%test _ = Lose = FlagPickEvaluator.eval 4
let%test _ = Lose = FlagPickEvaluator.eval 8
