-module(task2_4).
-export([start/0, proc/4]).

proc(Self, Others, [InterestedToken | Rest], Tokens) ->
	case lists:member(InterestedToken, Tokens) of
		true ->
			io:format("Process ~p does have interested token ~p.~n", [Self, InterestedToken]),
			proc(Self, Others, Rest, Tokens);
		false ->
			io:format("Process ~p does not have interested token ~p. Requesting from other processes.~n", [Self, InterestedToken]),
			[Pid ! {request, Self, InterestedToken} || Pid <- Others],
			receive_msg(Self, Others, [InterestedToken | Rest], Tokens)
	end;

proc(Self, Others, [], Tokens) ->
	io:format("Process ~p received all interested tokens.~n", [Self]),
	receive_msg(Self, Others, [], Tokens).

receive_msg(Self, Others, InterestedTokens, Tokens) ->
	receive
		{request, Pid, Token} ->
			case lists:member(Token, Tokens) of
				true ->
					io:format("Process ~p received request for token ~p. Sending to ~p.~n", [Self, Token, Pid]),
					Pid ! {response, Token},
					timer:sleep(1000),
					receive_msg(Self, Others, InterestedTokens, Tokens -- [Token]);
				false -> 
					io:format("Process ~p received request for token ~p. Ignoring.~n", [Self, Token]),
					receive_msg(Self, Others, InterestedTokens, Tokens)
			end;
		{response, Token} ->
			io:format("Process ~p received token ~p.~n", [Self, Token]),
			proc(Self, Others, InterestedTokens, Tokens ++ [Token])
	after
		5000 -> io:format("Process ~p exiting.~n", [Self])
	end.

start() ->
	register(p0, spawn(task2_4, proc, [p0, [p1, p2, p3], [t0, t1], [t0]])),
	register(p1, spawn(task2_4, proc, [p1, [p0, p2, p3], [t0, t1], [t1]])),
	register(p2, spawn(task2_4, proc, [p2, [p0, p1, p3], [t0, t1], []])),
	register(p3, spawn(task2_4, proc, [p3, [p0, p1, p2], [t0, t1], []])).
