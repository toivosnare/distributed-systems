-module(task4_1).
-export([ring_start/2, ring_join/3, ring_successor/2, ring_successor/3, ring_recover/3, ring_recover/2, send_next/3, ring/3, start/0]).

ring_start(Self, InterestedTokens) ->
	io:format("Process ~p starting new ring.~n", [Self]),
	ring(Self, InterestedTokens, [Self]).

ring_join(Self, InterestedTokens, Host) ->
	io:format("Process ~p joining ring through ~p.~n", [Self, Host]),
	Host ! {join, Self},
	receive {new_ring, Ring} -> 
		io:format("Process ~p joined ring ~p.~n", [Self, Ring]),
		ring(Self, InterestedTokens, Ring)
	after 1000 ->
		io:format("Process ~p joining ring timed out.~n", [Self])
	end.

ring_successor(Self, Others) ->
	[First | _] = Others,
	ring_successor(Self, Others, First).

ring_successor(Self, [First, Second | _], _) when Self == First -> Second;
ring_successor(Self, [_ | Rest], First) -> ring_successor(Self, Rest, First);
ring_successor(_, [], First) -> First.

ring_recover(_, Ring, Responses) when length(Ring) == length(Responses) -> Ring;
ring_recover(Self, Ring, Responses) ->
	receive {alive, Process} -> ring_recover(Self, Ring, [Process | Responses])
	after 1000 ->
		[P ! {new_ring, Responses} || P <- Responses, P /= Self],
		Responses
	end.
ring_recover(Self, Ring) ->
	io:format("Process ~p recovering ring.~n", [Self]),
	[catch P ! {are_you_alive, Self} || P <- Ring, P /= Self],
	ring_recover(Self, Ring, [Self]).

send_next(Self, Ring, Token) ->
	Next = ring_successor(Self, Ring),
	io:format("Process ~p sending token ~p to ~p.~n", [Self, Token, Next]),
	timer:sleep(100),
	case (catch Next ! {token, Token}) of
		{'EXIT', {badarg, _}} ->
			io:format("Process ~p send failed.~n", [Self]),
			NewRing = ring_recover(Self, Ring),
			send_next(Self, NewRing, Token);
		_ -> Ring
	end.

ring(Self, InterestedTokens, Ring) ->
	receive
		{join, Process} ->
			io:format("Process ~p received join request from ~p. New ring is ~p.~n", [Self, Process, [Process | Ring]]),
			[P ! {new_ring, [Process | Ring]} || P <- [Process | Ring], P /= Self],
			ring(Self, InterestedTokens, [Process | Ring]);
		{new_ring, NewRing} ->
			io:format("Process ~p received new ring ~p.~n", [Self, NewRing]),
			ring(Self, InterestedTokens, NewRing);
		{token, Token} ->
			case lists:member(Token, InterestedTokens) of
				true -> io:format("Process ~p received interested token ~p.~n", [Self, Token]);
				false -> io:format("Process ~p received uninterested token ~p.~n", [Self, Token])
			end,
			NewRing = send_next(Self, Ring, Token),
			ring(Self, InterestedTokens, NewRing);
		{are_you_alive, Process} ->
			io:format("Process ~p responding to are_you_alive message from ~p.~n", [Self, Process]),
			Process ! {alive, Self},
			ring(Self, InterestedTokens, Ring);
		quit ->
			io:format("Process ~p quitting.~n", [Self])
    after
        5000 ->
			io:format("Process ~p timed out.~n", [Self]),
            NewRing = ring_recover(Self, Ring),
            ring(Self, InterestedTokens, NewRing)
	end.

start() ->
	register(p1, spawn(task4_1, ring_start, [p1, [t1]])),
	register(p2, spawn(task4_1, ring_join, [p2, [], p1])),
	register(p3, spawn(task4_1, ring_join, [p3, [], p1])),
	register(p4, spawn(task4_1, ring_join, [p4, [], p1])),
	p1 ! {token, t1},
	timer:sleep(1000),
	p2 ! quit,
	timer:sleep(3000),
	[P ! quit || P <- [p1, p3, p4]].
