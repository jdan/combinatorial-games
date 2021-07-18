open Util

module type CombinatorialGame = sig
  type result = Win | Lose
  type game_state

  val next_states : game_state -> game_state list
  val is_loss : game_state -> bool
end

module CombinatorialGameEvaluator (G : CombinatorialGame) = struct
  open G

  (* todo: memoize *)
  let rec eval state =
    if is_loss state
    then Lose
    else
      let eval_next = next_states state |> List.map eval in
      (* SHAPE: Page 112 *)
      (* If every move I make leads to a W, my current position is an L. *)
      if all (fun r -> r = Win) eval_next
      then Lose
      (* If some move I can make leads to an L, my current position is a W. *)
      else Win
end

module SinglePileNim = struct
  type result = Win | Lose
  type game_state = int

  let next_states pile = range 0 (pile - 1)
  let is_loss = (=) 0
end

module SinglePileNimEvaluator = CombinatorialGameEvaluator(SinglePileNim)
let%test _ = SinglePileNim.Win = SinglePileNimEvaluator.eval 5
let%test _ = SinglePileNim.Win = SinglePileNimEvaluator.eval 4
let%test _ = SinglePileNim.Win = SinglePileNimEvaluator.eval 1
let%test _ = SinglePileNim.Lose = SinglePileNimEvaluator.eval 0

module MultiPileNim = struct
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
  let%test _ = [[0; 0]; [0; 1]] = next_states [0; 2]
  let%test _ = [[0; 2]; [1; 2]; [2; 0]; [2; 1]] = next_states [2; 2]

  let is_loss = all (fun n -> n = 0)
end

module MultiPileNimEvaluator = CombinatorialGameEvaluator(MultiPileNim)
let%test _ = MultiPileNim.Win = MultiPileNimEvaluator.eval [5]
let%test _ = MultiPileNim.Lose = MultiPileNimEvaluator.eval [2; 2]
let%test _ = MultiPileNim.Win = MultiPileNimEvaluator.eval [2; 3]
