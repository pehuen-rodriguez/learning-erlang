-module(bit_count).
-export([bits/1, bits_tail/1, test/0]).


% Count the number of ones in the binary representation of a number
% This can be done by iterative division by 2 and counting remainders.

% Direct recursion
bits(0) -> 0;
bits(N) when N > 0 ->
    bits(N div 2) + N rem 2.

% Tail recursion
bits_tail(N) when N >= 0 -> bits_tail(N, 0).

bits_tail(0, C) -> C;
bits_tail(N, C) -> bits_tail(N div 2, C + N rem 2).

test() ->
    0 = bits(0),
    0 = bits_tail(0),
    1 = bits(2048),
    1 = bits_tail(2048),
    10 = bits(1023),
    10 = bits_tail(1023),
    ok.