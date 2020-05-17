% Exercise 2.3
-module(pieces).
-export([test/0]).

pieces(0) -> 1;
pieces(N) -> N + pieces(N - 1).

test() ->
  4 = pieces(2),
  79 = pieces(12),
  ok.
