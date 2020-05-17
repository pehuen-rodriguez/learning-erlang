-module(fib_pattern_matching).
-export([fib/1]).

fib_p(0) -> {0, 1};
fib_p(N) ->
  io:format("invoking fib_p with: ~p~n", [N]),
  {Prev, Curr} = fib_p(N - 1),
  io:format("unfolding results: ~p~n", [Curr]),
  {Curr, Prev + Curr}.

fib(N) ->
  {Prev, _} = fib_p(N),
  Prev.
