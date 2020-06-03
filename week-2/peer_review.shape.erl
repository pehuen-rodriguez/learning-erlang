-module(shape).
-export([perimeter/1, area/1, enclosing/1, test/0]).

% In principle we do not care for the position in space of the polygons.
% We only care about calculating their area and their perimeter,
% therefore no need to input coordinates.

% CIRCLE and RECTANGLE 
% Calculating perimeter, area and smallest enclosing rectangle for rectangle 
% and circle is straightforward

% TRIANGLE
% We model a triangle with sides of lengths A, B and C with three parameters: 
% A, B and the angle between A and B.
% With this model:
%   - the perimeter is calculated with the law of cosines.
%   - the area can be calculated using Heron's formula.
%   - the angles are derived using the law of sines, but we don't use this.
% The enclosing rectangle is the one that has the longest side as one side and
% the height of the rectangle as another side. That is, a rectangle of 
% dimensions Base x Height = 2 x area of triangle.

perimeter({circle, R})               when R > 0        -> math:pi() * 2 * R;
perimeter({rectangle, {W, H}})       when W > 0, H > 0 -> 2 * W + 2 * H;
perimeter({triangle, {A, B, ArcAB}}) when A > 0, B > 0 ->
    ArcABrad = math:pi() * ArcAB / 180, % Erlang's math trigonometric functions
                                        % use radians instead of degrees
    C = math:sqrt(A * A + B * B - 2 * A * B * math:cos(ArcABrad)), % law of cosines
    A + B + C.

area({circle, R}) when R > 0 -> math:pi() * R * R;
area({rectangle, {W, H}}) when W > 0, H > 0 -> W * H;
area({triangle, {A, B, ArcAB}}) when A > 0, B > 0 -> 
    P = perimeter({triangle, {A, B, ArcAB}}),
    C = P - A -B,
    S = P / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)). % Heron's formula

enclosing({circle, R}) when R > 0 -> {rectangle, {2 * R, 2 * R}};
enclosing({rectangle, {W, H}}) when W > 0, H > 0 -> {rectangle, {W, H}};
enclosing({triangle, {A, B, ArcAB}}) when A > 0, B > 0 ->
    case {ArcAB == 90} of
        {true} -> % this case does not need any calculation
            {rectangle, {A, B}};
        {false} ->
            C = perimeter({triangle, {A, B, ArcAB}}) - A -B,
            Area = area({triangle, {A, B, ArcAB}}),
            % Now we have to see which is the longest side
            case {A >= B, A >= C, B >= C} of
                {true, true, _} -> % A is the Base
                    H = 2 * Area / A,
                    {rectangle, {A, H}};
                {true, false, _} -> % C is the Base
                    H = 2 * Area / C,
                    {rectangle, {C, H}};
                {false, _, true} -> % B is the Base
                     H = 2 * Area / B,
                    {rectangle, {B, H}}; 
                {false, _, false} -> % C is the Base
                    H = 2 * Area / C,
                    {rectangle, {C, H}}
            end
    end.

% TESTING

test_perimeter() ->
    A = 2 * math:pi(),
    A = perimeter({circle, 1}),
    B = 3 * A,
    B = perimeter({circle, 3}),
    18 = perimeter({rectangle, {6, 3}}),
    42 = perimeter({rectangle, {15, 6}}),
    6 = round(perimeter({triangle, {2, 2, 60}})),  % equilateral
    24 = round(perimeter({triangle, {8, 6, 90}})), % rectangle scalene
    99 = round(perimeter({triangle, {29, 29, 90}})), % rectangle isosceles
    35 = round(perimeter({triangle, {7, 14, 75.5225}})), % isosceles non-rectangle
    15 = round(perimeter({triangle, {5, 3, 120}})), % scalene with obtuse AB angle
    ok.

test_area() ->
    A = math:pi() * 9,
    A = area({circle, 3}),
    90 = area({rectangle, {15, 6}}),
    173 = round(100 * area({triangle, {2, 2, 60}})),  % equilateral
    24 = round(area({triangle, {8, 6, 90}})), % rectangle scalene
    4205 = round(10 * area({triangle, {29, 29, 90}})), % rectangle isosceles
    4744 = round(100 * area({triangle, {7, 14, 75.5225}})), % isosceles non-rectangle
    6495 = round(1000 * area({triangle, {5, 3, 120}})), % scalene with obtuse AB angle
    ok.

test_enclosing() ->
    {rectangle, {4, 4}} = enclosing({circle, 2}),
    {rectangle, {7, 3}} = enclosing({rectangle, {7, 3}}),
    % for triangles we use 2 digits precision for the testing
    {rectangle, {W0, H0}} = enclosing({triangle, {2, 2, 60}}),  % equilateral
    {rectangle, {2, 173}} = {rectangle, {W0, round(H0 * 100)}},
    {rectangle, {8, 6}} = enclosing({triangle, {8, 6, 90}}), % rectangle scalene
    {rectangle, {29, 29}} = enclosing({triangle, {29, 29, 90}}), % rectangle isosceles
    {rectangle, {W1, H1}} =  enclosing({triangle, {7, 14, 75.5225}}), % isosceles non-rectangle
    {rectangle, {14, 678}} = {rectangle, {round(W1), round(H1 * 100)}},
    {rectangle, {W2, H2}} =  enclosing({triangle, {5, 3, 120}}), % scalene with obtuse AB angle
    {rectangle, {7, 186}} = {rectangle, {round(W2), round(H2 * 100)}}, 
    ok.

test() ->
    ok = test_perimeter(),
    ok = test_area(),
    ok = test_enclosing(),
    ok.
