## combinatorial-games

An exploration of evaluating the results of [games without chance](https://en.wikipedia.org/wiki/Combinatorial_game_theory) under perfect play. [game.ml](/lib/game.ml) contains all the code.

Inspired by [Jordan Ellenberg's novel, Shape](https://amzn.to/3yKptm0) (Chapter 5: _"His Style Was Invincibility"_)

### example

```ocaml
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
end)
```

### running

tests

```
$ dune runtest
```

main executable (tbd)

```
$ dune exec bin/main.exe
```
