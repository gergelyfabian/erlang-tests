-module(ring).
-export([start/2, rpc/2]).

start(N, M) ->
  %% Start the first process separately.
  FirstPid = spawn(fun() -> process_start(1, N, void) end),
  %% Send a message to the first process to start the message loop.
  FirstPid ! {self(), {message, 0, M, "This is my message"}}.

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  end.

%% Make a process's setup, and start it's loop.
process_start(I, N, FirstPidOrig) ->
  io:format("Starting process #~p/~p (~p).~n" ,[I, N, self()]),
  %% Determine the first process's id
  %% If I is 1, then the FirstPid is self(), otherwise it's what was provided.
  case I of
    1 -> FirstPid = self();
    _ -> FirstPid = FirstPidOrig
  end,
  io:format("First pid is ~p in #~p/~p (~p).~n" ,[FirstPid, I, N, self()]),
  %% Start the next process, but just if it's not the end yet.
  case I of
    N ->
      %% We started processes from 0, when we reach N, do not start any more,
      %% and set the next process's id to the first.
      NextPid = FirstPid;
    _ ->
      %% Start the next process, providing an increased counter and the correct first process id.
      NextPid = spawn(fun() -> process_start(I+1, N, FirstPid) end)
  end,
  io:format("Next pid is ~p in #~p/~p (~p).~n" ,[NextPid, I, N, self()]),
  %% Start this process's loop, providing the first and the next process's id.
  process(I, N, FirstPid, NextPid).

process(I, N, FirstPid, NextPid) ->
  receive
    %% Receive a ring message.
    {From, {message, J, M, Contents}=Z} ->
      io:format("Received in #~p (~p) from ~p: ~p~n" ,[I, self(), From, Z]),
      %% Determine whether to increase the counter (so we know how many times the message has gone around).
      case I of
        %% This needs to be done just for the first element of the ring.
        1 -> J2 = J+1;
        _ -> J2 = J
      end,
      %% Check whether we need to stop sending messages.
      if
        (I==N) and (J2==M) ->
          %% Stop just if this is the last ring member, and have sent messages M times.
          io:format("Finished in #~p (~p), stopping ring.~n" ,[I, self()]),
          NextPid ! {self(), cancel};
        true ->
          %% Send the message to the next element otherwise.
          NextPid ! {self(), {message, J2, M, Contents}}
      end,
      process(I, N, FirstPid, NextPid);
    {From, cancel} ->
      io:format("Received cancel in #~p (~p) from ~p~n" ,[I, self(), From]),
      case I of
        %% Simply stop if this is the Nth process.
        N -> void;
        %% Also stop other processes if this is not the Nth.
        _ -> NextPid ! {self(), cancel}
      end;
    Any ->
      io:format("Received in #~p (~p): ~p~n" ,[I, self(), Any]),
      process(I, N, FirstPid, NextPid)
  end.
