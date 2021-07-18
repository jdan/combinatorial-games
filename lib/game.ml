open Util

type result = Win | Lose | Draw

module type CombinatorialGame = sig
  type game_state
  val next_states : game_state -> game_state list
  val immediate_result : game_state -> result option
end

module CombinatorialGameEvaluator (Game : CombinatorialGame) = struct
  (* todo: memoize *)
  let rec eval state =
    match Game.immediate_result state with
    | Some result -> result
    | None ->
      let eval_next = Game.next_states state |> List.map eval in
      (* SHAPE: Page 126 *)
      (* First Rule: If every move I make leads to a W, my current
         position is an L. *)
      if all (fun r -> r = Win) eval_next
      then Lose
      (* Second Rule: If some move I can make leads to an L, my
         current position is a W. *)
      else if any (fun r -> r = Lose) eval_next
      then Win
      (* Third Rule: If no move I can make leads to an L, but not
         every move I make leads to a W, my current position is a D. *)
      else Draw
end

module SinglePileNim = struct
  type game_state = int

  let immediate_result state =
    if state = 0
    then Some Lose
    else None

  let next_states pile = range 0 (pile - 1)
end

let%test_module _ = (module struct
  module M = CombinatorialGameEvaluator(SinglePileNim)
  let%test _ = Win = M.eval 5
  let%test _ = Win = M.eval 4
  let%test _ = Win = M.eval 1
  let%test _ = Lose = M.eval 0
end)

module MultiPileNim = struct
  type game_state = int list

  let immediate_result piles =
    if all (fun n -> n = 0) piles
    then Some Lose
    else None

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
end

let%test_module _ = (module struct
  module M = CombinatorialGameEvaluator(MultiPileNim)
  let%test _ = Win = M.eval [5]
  let%test _ = Lose = M.eval [2; 2]
  let%test _ = Win = M.eval [2; 3]
end)

module FlagPick = struct
  type game_state = int
  let immediate_result state =
    if state = 0
    then Some Lose
    else None

  (* You can pick 1, 2, or 3 flags *)
  let next_states n =
    if n < 4
    then range 0 (n - 1)
    else [n - 1; n - 2; n - 3]
end

let%test_module _ = (module struct
  module M = CombinatorialGameEvaluator(FlagPick)
  let%test _ = Win = M.eval 5
  let%test _ = Win = M.eval 3
  let%test _ = Win = M.eval 2
  let%test _ = Win = M.eval 1

  let%test _ = Lose = M.eval 0
  let%test _ = Lose = M.eval 4
  let%test _ = Lose = M.eval 8
end)

module TicTacToe = struct
  type cell = X | O | Empty
  type game_state =
    { xs_turn : bool
    ; board : cell * cell * cell *
              cell * cell * cell *
              cell * cell * cell
    }

  let immediate_result { xs_turn = xs_turn ; board = (c11, c12, c13, c21, c22, c23, c31, c32, c33) } =
    let line a b c = a = b && a = c && a <> Empty
    and result_of_turn winning_cell =
      if (xs_turn && winning_cell = X) || (not xs_turn && winning_cell = O)
      then Win
      else Lose
    in
    (* horizontal *)
    if line c11 c12 c13 then Some (result_of_turn c11)
    else if line c21 c22 c23 then Some (result_of_turn c21)
    else if line c31 c32 c33 then Some (result_of_turn c31)

    (* vertical *)
    else if line c11 c21 c31 then Some (result_of_turn c11)
    else if line c12 c22 c32 then Some (result_of_turn c12)
    else if line c13 c23 c33 then Some (result_of_turn c13)

    (* diagonal *)
    else if line c11 c22 c33 then Some (result_of_turn c11)
    else if line c13 c22 c31 then Some (result_of_turn c13)

    (* full board *)
    else if all ((<>) Empty) [c11; c12; c13; c21; c22; c23; c31; c32; c33]
    then Some Draw
    else None

  exception StateException
  (* Depending on whose turn it is, place a mark in every empty square *)
  let next_states { xs_turn = xs_turn ; board = (c11, c12, c13, c21, c22, c23, c31, c32, c33) } =
    let placement = if xs_turn then X else O
    and board_ls = [c11; c12; c13; c21; c22; c23; c31; c32; c33]
    in
    board_ls
    |> List.mapi (
      fun idx cell ->
        if cell = Empty
        then Some (update idx placement board_ls)
        else None
    )
    |> List.filter_map (fun x -> x)
    |> List.map (fun board ->
        match board with
        | [c11; c12; c13; c21; c22; c23; c31; c32; c33] ->
          { xs_turn = not xs_turn
          ; board = (c11, c12, c13, c21, c22, c23, c31, c32, c33)
          }
        | _ -> raise StateException
      )
end

let%test_module _ = (module struct
  module M = CombinatorialGameEvaluator(TicTacToe)

  (* An empty board is a Draw *)
  let%test _ = Draw = M.eval { xs_turn = true; 
                               board = (Empty, Empty, Empty, 
                                        Empty, Empty, Empty,
                                        Empty, Empty, Empty) 
                             }

  (* In fact, after X plays the top left it's still a draw *)
  let%test _ = Draw = M.eval { xs_turn = false; 
                               board = (X, Empty, Empty, 
                                        Empty, Empty, Empty, 
                                        Empty, Empty, Empty) 
                             }

  (* O top-left is a mistake! *)
  let%test _ = Win = M.eval { xs_turn = true; 
                              board = (X, Empty, O, 
                                       Empty, Empty, Empty,
                                       Empty, Empty, Empty)
                            }
  (* As-is top-middle *)
  let%test _ = Win = M.eval { xs_turn = true;
                              board = (X, O, Empty,
                                       Empty, Empty, Empty,
                                       Empty, Empty, Empty)
                            }
  (* ... and any corner... *)
  let%test _ = Win = M.eval { xs_turn = true;
                              board = (X, Empty, Empty,
                                       Empty, Empty, Empty,
                                       Empty, Empty, O)
                            }

  (* O _must_ play in the center to prevent a win *)
  let%test _ = Draw = M.eval { xs_turn = true;
                               board = (X, Empty, Empty,
                                        Empty, O, Empty,
                                        Empty, Empty, Empty)
                             }

  (* O can, of course, turn it around if X plays poorly *)
  let%test _ = Win = M.eval { xs_turn = false;
                              board = (X, Empty, Empty,
                                       O, O, Empty,
                                       X, Empty, X)
                            }
end)
