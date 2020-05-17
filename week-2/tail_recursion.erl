% Exercise 2.5
-module(tail_recursion).
-export([test_fib/0, test_perfect/0]).

fib(N) -> fib(N, 0, 1).

fib(0, Prev, _Current) -> Prev;
fib(N, Prev, Current) -> fib(N - 1, Current, Prev + Current).

% A positive integer is perfect when it is the sum of its divisors, e.g. 6=1+2+3, 28=1+2+4+7+14.
% could start at 1 and go up to Number or start at Number-1 and go down to 0.
perfect(0) -> false;
perfect(Number) when Number > 0 -> perfect(Number-1, 0, Number).

perfect(0, Acc, Number) -> Acc == Number;
perfect(Divisor, Acc, Number) when Number rem Divisor == 0 -> perfect(Divisor - 1, Acc+Divisor, Number);
perfect(Divisor, Acc, Number) -> perfect(Divisor - 1, Acc, Number).

test_fib() ->
  5 = fib(5),
  55 = fib(10),
  144 = fib(12),
  ok.

test_perfect() ->
  true = perfect(6),
  true = perfect(28),
  false = perfect(99),
  ok.
