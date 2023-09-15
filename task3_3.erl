-module(task3_3).
-export([s/4, start/0]).

s(SelfIndex, Next, State, CoordinatorIndex) ->
	receive
		{election, start} ->
			io:format("Process s~p starting election.~n", [SelfIndex]),
			Next ! {election, SelfIndex},
			s(SelfIndex, Next, election, 0);
		{election, ProposedCoordinatorIndex} ->
			if
				ProposedCoordinatorIndex == SelfIndex ->
					io:format("Process s~p received election message with it's own index. Sending coordinator message.~n", [SelfIndex]),
					Next ! {coordinator, SelfIndex},
					s(SelfIndex, Next, normal, SelfIndex);
				ProposedCoordinatorIndex > SelfIndex ->
					io:format("Process s~p received election message with greater index. Sending forward.~n", [SelfIndex]),
					Next ! {election, ProposedCoordinatorIndex},
					s(SelfIndex, Next, election, CoordinatorIndex);
				(ProposedCoordinatorIndex < SelfIndex) and (State /= election) ->
					io:format("Process s~p received election message with lesser index. Sending election message with own index.~n", [SelfIndex]),
					Next ! {election, SelfIndex},
					s(SelfIndex, Next, State, CoordinatorIndex);
				true ->
					io:format("Process s~p ignoring election message.~n", [SelfIndex]),
					s(SelfIndex, Next, State, CoordinatorIndex)
			end;
		{coordinator, ProposedCoordinatorIndex} ->
			case ProposedCoordinatorIndex == SelfIndex of
				true ->
					io:format("Process s~p received coordinator message with it's own index. Sending quit messages.~n", [SelfIndex]),
					[S ! quit || S <- [s1, s2, s3, s4]],
					s(SelfIndex, Next, State, CoordinatorIndex);
				false ->
					io:format("Process s~p received coordinator message. Sending forward.~n", [SelfIndex]),
					Next ! {coordinator, ProposedCoordinatorIndex},
					s(SelfIndex, Next, normal, ProposedCoordinatorIndex)
			end;
		quit ->
			io:format("Process s~p exiting.~n", [SelfIndex])
	end.

start() ->
	register(s1, spawn(task3_3, s, [1, s2, normal, 0])),
	register(s2, spawn(task3_3, s, [2, s3, normal, 0])),
	register(s3, spawn(task3_3, s, [3, s4, normal, 0])),
	register(s4, spawn(task3_3, s, [4, s1, normal, 0])),
	s1 ! {election, start}.
