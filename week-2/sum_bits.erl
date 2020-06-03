% Exercise 2.8 second part.
% Define a function bits/1 that takes a positive integer N and returns the sum of the bits in the binary
% representation. For example bits(7) is 3 and bits(8) is 1.
% See whether you can make both a direct recursive and a tail recursive definition.
% Which do you think is better? Why?
-module(sum_bits).
-export([test/0]).

% Tail recursion.
% I think tail recursion is better. Both easier to read and think of.
% It should also be faster.
bits(N) when N > 0      -> bits(N, 0).
bits(1, Acc)            -> Acc + 1;
bits(N, Acc)            -> bits(N div 2, Acc + (N rem 2)).

% Direct recursion. Should be a bit slower.
bits_direct(0)            -> 0;
bits_direct(1)            -> 1;
bits_direct(N) when N > 0 -> bits_direct(N div 2) + N rem 2.

test() ->
  1 = bits(8),         % 1000
  3 = bits(7),         % 111
  3 = bits(35),        % 100011
  7 = bits(949),       % 1110110101
  1 = bits_direct(8),  % 1000
  3 = bits_direct(7),  % 111
  3 = bits_direct(35), % 100011
  2 = bits_direct(36), % 100100
  ok.
