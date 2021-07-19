open Util

type result = Win | Lose | Draw

module type CombinatorialGame = sig
  type game_state
  val next_states : game_state -> game_state list
  val immediate_result : game_state -> result option
end

module CombinatorialGameEvaluator (Game : CombinatorialGame) = struct
  let eval_no_rec eval state =
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
  let eval = memo_rec eval_no_rec


  let best_move state =
    (* A WIN means your move makes your opponent LOSE, so we
       flip the result when displaying the best move's state *)
    let flip_result = function
      | Lose -> Win
      | Win -> Lose
      | Draw -> Draw
    in let eval_next =
         Game.next_states state
         |> List.map (fun st -> (st, eval st |> flip_result))
    in
    first (fun (_, res) -> res = Win) eval_next
    ||* first (fun (_, res) -> res = Draw) eval_next
    ||* first (fun (_, res) -> res = Lose) eval_next
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
  let%test "Take all 5" = Win = M.eval 5
  let%test "Take all 4" = Win = M.eval 4
  let%test "Take the remaining stone" = Win = M.eval 1
  let%test "You lose" = Lose = M.eval 0
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
  let%test "Take all stones in the pile" = Win = M.eval [5]
  let%test "Leave the piles as 2, 2" = Win = M.eval [2; 3]
  let%test "Your opponent can force your loss by mimicking" = Lose = M.eval [2; 2]

  let%test "Leave the piles as 2, 2" = Some ([2; 2], Win) = M.best_move [2; 3]
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

  let%test "No flags left is a loss" = Lose = M.eval 0
  let%test "Four flags allows your opponent to force a loss" = Lose = M.eval 4
  let%test "Any multiple of four flags is a loss" = Lose = M.eval 8
end)

module TicTacToe = struct
  type cell = X | O | Empty
  type game_state =
    { xs_turn : bool
    ; board : cell * cell * cell *
              cell * cell * cell *
              cell * cell * cell
    }

  let immediate_result { xs_turn ; board = (c11, c12, c13, c21, c22, c23, c31, c32, c33) } =
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
  let next_states { xs_turn ; board = (c11, c12, c13, c21, c22, c23, c31, c32, c33) } =
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

  let%test "An empty board is a Draw" =
    Draw = M.eval { xs_turn = true;
                    board = (Empty, Empty, Empty,
                             Empty, Empty, Empty,
                             Empty, Empty, Empty)
                  }

  let%test "O can force a draw even if X plays correctly" =
    Draw = M.eval { xs_turn = false;
                    board = (X, Empty, Empty,
                             Empty, Empty, Empty,
                             Empty, Empty, Empty)
                  }

  let%test "O playing top-right is a fatal error" =
    Win = M.eval { xs_turn = true;
                   board = (X, Empty, O,
                            Empty, Empty, Empty,
                            Empty, Empty, Empty)
                 }

  let%test "O playing center edge loses" =
    Win = M.eval { xs_turn = true;
                   board = (X, O, Empty,
                            Empty, Empty, Empty,
                            Empty, Empty, Empty)
                 }

  let%test "O playing any corner loses" =
    Win = M.eval { xs_turn = true;
                   board = (X, Empty, Empty,
                            Empty, Empty, Empty,
                            Empty, Empty, O)
                 }

  let%test "O _must_ play in the center to prevent a win" =
    Draw = M.eval { xs_turn = true;
                    board = (X, Empty, Empty,
                             Empty, O, Empty,
                             Empty, Empty, Empty)
                  }

  let%test "O can, of course, turn it around if X plays poorly" =
    Win = M.eval { xs_turn = false;
                   board = (X, Empty, Empty,
                            O, O, Empty,
                            X, Empty, X)
                 }

  let%test "The best move for O is to play in the center" =
    Some ( { TicTacToe.xs_turn = true
           ; board = (X, Empty, Empty,
                      Empty, O, Empty,
                      Empty, Empty, Empty)
           }
         , Draw
         ) =
    M.best_move
      { xs_turn = false
      ; board = (X, Empty, Empty,
                 Empty, Empty, Empty,
                 Empty, Empty, Empty)
      }
end)

module HareAndHounds = struct
  type cell = Empty | Hound | Hare

  (* https://upload.wikimedia.org/wikipedia/commons/8/85/Hare_and_Hounds_board.png *)
  type board = cell * cell * cell *
               cell * cell * cell * cell * cell *
               cell * cell * cell

  type game_state =
    { hounds_turn: bool
    ; board : board
    ; vertical_move_streak: int
    }

  let cell_list_of_board ((t1, t2, t3, m1, m2, m3, m4, m5, b1, b2, b3) : board) =
    [t1; t2; t3; m1; m2; m3; m4; m5; b1; b2; b3]

  exception BoardOfCellListMalformedException
  let board_of_cell_list : cell list -> board = function
    | [t1; t2; t3; m1; m2; m3; m4; m5; b1; b2; b3] -> (t1, t2, t3, m1, m2, m3, m4, m5, b1, b2, b3)
    | _ -> raise BoardOfCellListMalformedException

  let board_nth ((t1, t2, t3, m1, m2, m3, m4, m5, b1, b2, b3) : board) =
    List.nth [t1; t2; t3; m1; m2; m3; m4; m5; b1; b2; b3]

  let board_swap a b board =
    let  tmp = board_nth board a
    in
    cell_list_of_board board
    |> update a (board_nth board b)
    |> update b tmp
    |> board_of_cell_list

  exception MissingHareException
  let get_hare_index board =
    match
      board
      |> cell_list_of_board
      |> firsti ((=) Hare)
    with
    | None -> raise MissingHareException
    | Some idx -> idx

  let get_sorted_hound_indices board =
    board
    |> cell_list_of_board
    |> find_alli ((=) Hound)
    |> List.sort ((-))

  let hare_reached_goal = function
    | (_, _, _,
       Hare, _, _, _,
       _, _, _, _) -> true
    | _ -> false

  let hare_surrounded = function
    | (Empty, Empty, Hound,
       Empty, Empty, Empty, Hound, Hare,
       Empty, Empty, Hound) -> true
    | (Hound, Hare, Hound,
       Empty, Empty, Hound, Empty, Empty,
       Empty, Empty, Empty) -> true
    |  (Empty, Empty, Empty,
        Empty, Empty, Hound, Empty, Empty,
        Hound, Hare, Hound) -> true
    | _ -> false

  let vertical_move_limit = 10

  let immediate_result { hounds_turn; board; vertical_move_streak } =
    if vertical_move_streak >= vertical_move_limit
    then Some (if hounds_turn then Lose else Win)
    else if hare_reached_goal board
    then Some (if hounds_turn then Lose else Win)
    else if hare_surrounded board
    then Some (if hounds_turn then Win else Lose)
    else None

  exception InvalidIndexException
  (*  . 0 1 2
      3 4 5 6 7
        8 9 10
  *)
  let hare_idx_adjacency = function
    | 0 -> [1; 3; 4; 5]
    | 1 -> [0; 2; 5]
    | 2 -> [1; 5; 6; 7]
    | 3 -> [0; 4; 8]
    | 4 -> [0; 3; 5; 8]
    | 5 -> [0; 1; 2; 4; 6; 8; 9; 10]
    | 6 -> [2; 5; 7; 10]
    | 7 -> [2; 6; 10]
    | 8 -> [3; 4; 5; 9]
    | 9 -> [5; 8; 10]
    | 10 -> [5; 6; 7; 9]
    | _ -> raise InvalidIndexException

  let hound_idx_adjacency = function
    | 0 -> [1; 4; 5]
    | 1 -> [2; 5]
    | 2 -> [6; 7]
    | 3 -> [0; 4; 8]
    | 4 -> [0; 5; 8]
    | 5 -> [1; 2; 6; 9; 10]
    | 6 -> [2; 7; 10]
    | 7 -> []
    | 8 -> [4; 5; 9]
    | 9 -> [5; 10]
    | 10 -> [6; 7]
    | _ -> raise InvalidIndexException

  let vertical_moves = function
    | 0 -> [4]
    | 1 -> [5]
    | 2 -> [6]
    | 3 -> []
    | 4 -> [0; 8]
    | 5 -> [1; 9]
    | 6 -> [2; 10]
    | 7 -> []
    | 8 -> [4]
    | 9 -> [5]
    | 10 -> [6]
    | _ -> raise InvalidIndexException

  let next_states { hounds_turn ; board; vertical_move_streak } =
    if hounds_turn
    then
      get_sorted_hound_indices board
      |> List.map (fun hound_idx ->
          hound_idx_adjacency hound_idx
          |> List.filter (fun idx -> Empty = board_nth board idx)
          |> List.map (fun idx ->
              { hounds_turn = false
              ; board = board_swap hound_idx idx board
              ; vertical_move_streak =
                  let is_vertical_move = any ((=) idx) (vertical_moves hound_idx)
                  in
                  if is_vertical_move then vertical_move_streak + 1
                  else 0
              }
            )
        )
      |> List.concat
    else
      let hare_idx = get_hare_index board
      in
      hare_idx_adjacency hare_idx
      |> List.filter (fun idx -> Empty = board_nth board idx)
      |> List.map (fun idx ->
          { hounds_turn = true
          ; board = board_swap hare_idx idx board
          ; vertical_move_streak
          }
        )
end

let%test_module "HareAndHounds" = (module struct
  module M = CombinatorialGameEvaluator(HareAndHounds)
  let%test "Hare wins if it gets to the left" =
    Win = M.eval { hounds_turn = false
                 ; board = (Empty, Empty, Empty,
                            Hare, Empty, Empty, Empty, Empty,
                            Empty, Empty, Empty)
                 ; vertical_move_streak = 0
                 }

  let%test "A trapped hare loses" =
    Lose = M.eval { hounds_turn = false
                  ; board = (Empty, Empty, Hound,
                             Empty, Empty, Empty, Hound, Hare,
                             Empty, Empty, Hound)
                  ; vertical_move_streak = 0
                  }

  let%test "A hare trapped on top loses" =
    Lose = M.eval { hounds_turn = false
                  ; board = (Hound, Hare, Hound,
                             Empty, Empty, Hound, Empty, Empty,
                             Empty, Empty, Empty)
                  ; vertical_move_streak = 0
                  }

  let%test "A hare trapped at the bottom loses" =
    Lose = M.eval { hounds_turn = false
                  ; board = (Empty, Empty, Empty,
                             Empty, Empty, Hound, Empty, Empty,
                             Hound, Hare, Hound)
                  ; vertical_move_streak = 0
                  }

  let%test "10 or move vertical moves in a row is a hare win" =
    Win = M.eval { hounds_turn = false
                 ; board = (Hound, Empty, Empty,
                            Hound, Empty, Empty , Empty, Hare,
                            Hound, Empty, Empty)
                 ; vertical_move_streak = 20
                 }

  let%test "The starting board is a win for the hounds" =
    Win = M.eval { hounds_turn = true
                 ; board = (Hound, Empty, Empty,
                            Hound, Empty, Empty , Empty, Hare,
                            Hound, Empty, Empty)
                 ; vertical_move_streak = 0
                 }
end)
