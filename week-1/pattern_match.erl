-module(pattern_match).
-export([test/0]).

% Exercise 1.15
% “exclusive or”. Give at least three others
xor_1(false, A) ->
  A;
xor_1(true, A) ->
  not A.

xor_2(A, B) ->
  not A==B.

xor_3(A, B) ->
  A=/=B.

% takes three integers and returns the maximum of the three
max_three(A, B, C) ->
  max(max(A, B), C).

% takes three integers and returns an integer, counting how
% many of its three arguments are equal
how_many_are_equal(_A, _A, _A) -> 3;
how_many_are_equal(_, _A, _A) -> 2;
how_many_are_equal(_A, _, _A) -> 2;
how_many_are_equal(_A, _A, _) -> 2;
how_many_are_equal(_, _, _) -> 0.

test() ->
  true = xor_1(true, false),
  true = xor_1(false, true),
  false = xor_1(false, false),
  false = xor_1(true, true),

  true = xor_2(true, false),
  true = xor_2(false, true),
  false = xor_2(false, false),
  false = xor_2(true, true),

  true = xor_3(true, false),
  true = xor_3(false, true),
  false = xor_3(false, false),
  false = xor_3(true, true),

  3 = max_three(3,2,1),
  4 = max_three(3,4,1),
  6 = max_three(3,4,6),

  2 = how_many_are_equal(3,2,3),
  2 = how_many_are_equal(3,3,2),
  2 = how_many_are_equal(2,3,3),
  3 = how_many_are_equal(3,3,3),
  0 = how_many_are_equal(1,2,3),
  ok.
