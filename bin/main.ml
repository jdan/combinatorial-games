open Lib.Game

module M = CombinatorialGameEvaluator(HareAndHounds)

let () =
  { HareAndHounds.hounds_turn = true
  ; board = (Hound, Empty, Empty,
             Hound, Empty, Empty , Empty, Hare,
             Hound, Empty, Empty)
  ; vertical_move_streak = 0
  }
  |> M.ideal_play
  |> List.map HareAndHounds.string_of_game_state
  |> String.concat "\n"
  |> print_endline
