% Excercise 1.9
-module(second).
-export([hypotenuse/2, permiter/2, area/2]).

% Adjacent and Opposite sides
hypotenuse(A, O) ->
  math:sqrt(first:square(A) + first:square(O)).

permiter(A, O) ->
  A + O + hypotenuse(A, O).

% Base and Height
area(B, H) ->
  first:mult(first:mult(B, H), 1/2).
