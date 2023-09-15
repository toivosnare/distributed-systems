-module(task4_2).
-export([process/1, ring_start/3, ring_join/4, ring_successor/2, ring_successor/3, ring_recover/3, ring_recover/2, send_next/3, ring/4, start/0]).

process(Index) ->
    case Index of
        1 -> p1;
        2 -> p2;
        3 -> p3;
        4 -> p4
    end.

ring_start(SelfIndex, State, CoordinatorIndex) ->
	io:format("Process p~p starting new ring.~n", [SelfIndex]),
	ring(SelfIndex, State, CoordinatorIndex, [SelfIndex]).

ring_join(SelfIndex, State, CoordinatorIndex, HostIndex) ->
	io:format("Process p~p joining ring through p~p.~n", [SelfIndex, HostIndex]),
	process(HostIndex) ! {join, SelfIndex},
	receive {new_ring, Ring} -> 
		io:format("Process p~p joined ring p~p.~n", [SelfIndex, Ring]),
		ring(SelfIndex, State, CoordinatorIndex, Ring)
	after 1000 ->
		io:format("Process p~p joining ring timed out.~n", [SelfIndex])
	end.

ring_successor(SelfIndex, Others) ->
	[First | _] = Others,
	ring_successor(SelfIndex, Others, First).

ring_successor(SelfIndex, [First, Second | _], _) when SelfIndex == First -> Second;
ring_successor(SelfIndex, [_ | Rest], First) -> ring_successor(SelfIndex, Rest, First);
ring_successor(_, [], First) -> First.

ring_recover(_, Ring, Responses) when length(Ring) == length(Responses) -> Ring;
ring_recover(SelfIndex, Ring, Responses) ->
	receive {alive, Process} -> ring_recover(SelfIndex, Ring, [Process | Responses])
	after 1000 ->
		[process(P) ! {new_ring, Responses} || P <- Responses, P /= SelfIndex],
		Responses
	end.

ring_recover(SelfIndex, Ring) ->
	io:format("Process p~p recovering ring.~n", [SelfIndex]),
	[catch process(P) ! {are_you_alive, SelfIndex} || P <- Ring, P /= SelfIndex],
	ring_recover(SelfIndex, Ring, [SelfIndex]).

send_next(SelfIndex, Ring, Message) ->
    NextIndex = ring_successor(SelfIndex, Ring),
	case (catch process(NextIndex) ! Message) of
		{'EXIT', {badarg, _}} ->
            NewRing = ring_recover(SelfIndex, Ring),
            send_next(SelfIndex, NewRing, Message);
		_ -> Ring
	end.

ring(SelfIndex, State, CoordinatorIndex, Ring) ->
	receive
		{join, Index} ->
			io:format("Process p~p received join request from ~p. New ring is ~p.~n", [SelfIndex, Index, [Index | Ring]]),
			[process(P) ! {new_ring, [Index | Ring]} || P <- [Index | Ring], P /= SelfIndex],
			ring(SelfIndex, State, CoordinatorIndex, [Index | Ring]);
		{new_ring, NewRing} ->
			io:format("Process p~p received new ring ~p.~n", [SelfIndex, NewRing]),
			ring(SelfIndex, State, CoordinatorIndex, NewRing);
		{election, start} ->
			io:format("Process p~p starting election.~n", [SelfIndex]),
            NewRing = send_next(SelfIndex, Ring, {election, SelfIndex}),
            ring(SelfIndex, election, 0, NewRing);
		{election, ProposedCoordinatorIndex} ->
			if
				ProposedCoordinatorIndex == SelfIndex ->
					io:format("Process p~p received election message with it's own index. Sending coordinator message.~n", [SelfIndex]),
                    NewRing = send_next(SelfIndex, Ring, {coordinator, SelfIndex}),
                    ring(SelfIndex, normal, SelfIndex, NewRing);
				ProposedCoordinatorIndex > SelfIndex ->
					io:format("Process p~p received election message with greater index. Sending forward.~n", [SelfIndex]),
                    NewRing = send_next(SelfIndex, Ring, {election, ProposedCoordinatorIndex}),
                    ring(SelfIndex, election, CoordinatorIndex, NewRing);
				(ProposedCoordinatorIndex < SelfIndex) and (State /= election) ->
					io:format("Process p~p received election message with lesser index. Sending election message with own index.~n", [SelfIndex]),
                    NewRing = send_next(SelfIndex, Ring, {election, SelfIndex}),
                    ring(SelfIndex, State, CoordinatorIndex, NewRing);
				true ->
					io:format("Process p~p ignoring election message.~n", [SelfIndex]),
                    ring(SelfIndex, State, CoordinatorIndex, Ring)
			end;
		{coordinator, ProposedCoordinatorIndex} ->
			case ProposedCoordinatorIndex == SelfIndex of
				true ->
					io:format("Process p~p received coordinator message with it's own index. Sending quit messages.~n", [SelfIndex]),
					[catch P ! quit || P <- [p1, p2, p3, p4]],
					ring(SelfIndex, State, CoordinatorIndex, Ring);
				false ->
					io:format("Process p~p received coordinator message. Sending forward.~n", [SelfIndex]),
                    NewRing = send_next(SelfIndex, Ring, {coordinator, ProposedCoordinatorIndex}),
                    ring(SelfIndex, normal, ProposedCoordinatorIndex, NewRing)
			end;
		{are_you_alive, Index} ->
			io:format("Process p~p responding to are_you_alive message from p~p.~n", [SelfIndex, Index]),
			process(Index) ! {alive, SelfIndex},
			ring(SelfIndex, State, CoordinatorIndex, Ring);
		quit ->
			io:format("Process p~p quitting.~n", [SelfIndex])
    after
        5000 ->
			io:format("Process p~p timed out.~n", [SelfIndex]),
            NewRing = ring_recover(SelfIndex, Ring),
            ring(SelfIndex, State, CoordinatorIndex, NewRing)
	end.

start() ->
	register(p1, spawn(task4_2, ring_start, [1, normal, 0])),
	register(p2, spawn(task4_2, ring_join, [2, normal, 0, 1])),
	register(p3, spawn(task4_2, ring_join, [3, normal, 0, 1])),
	register(p4, spawn(task4_2, ring_join, [4, normal, 0, 1])),
    timer:sleep(1000),
	p1 ! quit,
    timer:sleep(1000),
	p2 ! {election, start}.
