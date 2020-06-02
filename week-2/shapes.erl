% Exercise 2.8
% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
% Choose a suitable representation of triangles, and augment area/1 and perimeter/1 to handle this case too.
% Define a function enclose/1 that takes a shape and returns the smallest enclosing rectangle of the shape.
-module(shapes).
-compile({no_auto_import, [max/2]}).
-export([test/0]).

perimeter({rectangle, Base, Height}) when (Base > 0) and (Height > 0) ->
  2 * (Base + Height);
perimeter({circle, Radius}) when Radius > 0 ->
  2 * math:pi() * Radius;
perimeter({triangle, A, B, C}) ->
  A + B + C.

area({rectangle, Base, Height}) when (Base > 0) and (Height > 0) ->
  Base * Height;
area({circle, Radius}) when Radius > 0 ->
  math:pi() * math:pow(Radius, 2);
% If I've got sides I can use semiperimeter and Heron's
area(Triangle = {triangle, A, B, C}) ->
  Semi = perimeter(Triangle) / 2,
  math:sqrt(Semi * (Semi - A) * (Semi - B) * (Semi - C));
% I could also be given base and height
area({triangle, Base, Height}) ->
  Base * Height / 2.
% To distiguish two sides impl (and use Pythagora's) from aboves Base and Height impl
% I should've used a different function name

% Smallest enclosing rectangle for circle is of it's diameter
enclose({circle, Radius}) when Radius > 0 ->
  Diameter = Radius * 2,
  {rectangle, Diameter, Diameter};
enclose(Triangle = {triangle, A, B, C}) ->
  Area = area(Triangle),
  Base = max([A, B, C]),
  Height = (Area * 2) / Base,
  {rectangle, Base, Height};
enclose({triangle, Base, Height}) ->
  {rectangle, Base, Height}.

% Lists implementation for max
max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.

test() ->
  12 = perimeter({rectangle, 2, 4}),
  13.0 = perimeter({rectangle, 1, 5.5}),
  31.41592653589793 = perimeter({circle, 5}),
  75.39822368615503 = perimeter({circle, 12}),
  21 = perimeter({triangle, 4, 8, 9}),
  20 = area({rectangle, 10, 2}),
  10.0 = area({circle, 1.784124116152771114538966372565082590394202920572}), % :)
  10.0 = area({triangle, 10, 2}),
  {rectangle, 10, 10} = enclose({circle, 5}),
  {rectangle, 9, 3.5551215012835904} = enclose({triangle, 4, 8, 9}),
  {rectangle, 12, 20} = enclose({triangle, 12, 20}),
  ok.
