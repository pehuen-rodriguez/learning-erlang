% Exercise 2.3
-module(fibonacci).
-export([test/0]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 0 ->
  fib(N-2) + fib(N-1).

test() ->
  5 = fib(5),
  55 = fib(10),
  144 = fib(12),
  ok.
