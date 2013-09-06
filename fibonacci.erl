-module(fibonacci).
-export([fibonacci/1]).

%% Count the Nth Fibonacci number in a "recursive" way, but not by doing
%% two recursions from each level (as in the traditional definition), 'cause
%% that would mean calling the fibonacci() function more than once for most of the Ns.
%%
%% Let's do it the way the solution is presented in Python and Ruby books,
%% but in an Erlang-compatible way, with tail-recursion.

%% Externally visible functions masking how it works internally.
fibonacci(1) ->
  0;
fibonacci(2) ->
  1;
fibonacci(N) ->
  fibonacci(2, N, fibonacci(1), fibonacci(2)).

%% Internal functions doing the real calculation.
fibonacci(N, N, _A, B) ->
  B;

fibonacci(I, N, A, B) ->
  A1 = B,
  B1 = A+B,
  fibonacci(I+1, N, A1, B1).
