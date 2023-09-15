-module(task2_3).
-export([start/0, proc/4]).

proc(Self, Next, InterestedTokenIDs, [Token | Tail]) ->
	{TokenID, TokenTTL} = Token,
	case lists:member(TokenID, InterestedTokenIDs) of
		true -> io:format("Process ~p received interested token ~p.~n", [Self, Token]);
		false -> io:format("Process ~p received uninterested token ~p.~n", [Self, Token])
	end,
	timer:sleep(1000),
	Next ! {TokenID, TokenTTL - 1},
	proc(Self, Next, InterestedTokenIDs, Tail);

proc(Self, Next, InterestedTokenIDs, []) ->
	receive
		{TokenID, TokenTTL} when TokenTTL > 0 ->
			proc(Self, Next, InterestedTokenIDs, [{TokenID, TokenTTL}]);
		_ -> io:format("Process ~p exiting.~n", [Self])
	end.

start() ->
	register(p0, spawn(task2_3, proc, [p0, p1, [t0], [{t3, 10}]])),
	register(p1, spawn(task2_3, proc, [p1, p2, [t1], [{t2, 10}]])),
	register(p2, spawn(task2_3, proc, [p2, p3, [t2], [{t1, 10}]])),
	register(p3, spawn(task2_3, proc, [p3, p0, [t3], [{t0, 10}]])).
