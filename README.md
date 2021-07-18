## combinatorial-games

An exploration of evaluating the results of [games without chance](https://en.wikipedia.org/wiki/Combinatorial_game_theory) under perfect play.

[game.ml](/lib/game.ml) contains all the code.

### example

```ocaml
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

  (* O top-right is a mistake! *)
  let%test _ = Win = M.eval { xs_turn = true;
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
