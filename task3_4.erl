-module(task3_4).
-export([s/4, start/0]).

process(Index) ->
	case Index of
		1 -> s1;
		2 -> s2;
		3 -> s3;
		4 -> s4
	end.

all_process_indices() -> [1, 2, 3, 4].

s(SelfIndex, Timeout, State, VictoryTime) ->
	case State of
		start ->
			io:format("Process s~p starting election. Sending election message to processes with higher process IDs.~n", [SelfIndex]),
			[process(I) ! {election, SelfIndex} || I <- all_process_indices(), I > SelfIndex],
			V = erlang:monotonic_time(millisecond) + Timeout,
			s(SelfIndex, Timeout, wait, V);
		wait ->
			T = VictoryTime - erlang:monotonic_time(millisecond),
			receive
				{alive, SenderIndex} ->
					io:format("Process s~p received alive message from process s~p. Going to normal state.~n", [SelfIndex, SenderIndex]),
					s(SelfIndex, Timeout, normal, 0);
				{election, SenderIndex} ->
					io:format("Process s~p received election message from process s~p. Sending alive message.~n", [SelfIndex, SenderIndex]),
					process(SenderIndex) ! {alive, SelfIndex},
					s(SelfIndex, Timeout, wait, VictoryTime)
			after
				T ->
					io:format("Process s~p did not receive alive message in the timeout. Sending victory message to other processes and exiting.~n", [SelfIndex]),
					[process(I) ! {victory, SelfIndex} || I <- all_process_indices(), I /= SelfIndex]
			end;
		normal ->
			receive
				{election, SenderIndex} ->
					io:format("Process s~p received election message from process s~p. Sending alive message.~n", [SelfIndex, SenderIndex]),
					process(SenderIndex) ! {alive, SelfIndex},
					s(SelfIndex, Timeout, start, 0);
				{victory, SenderIndex} ->
					io:format("Process s~p received victory message from process s~p. Exiting.~n", [SelfIndex, SenderIndex])
			end
	end.

start() ->
	register(s1, spawn(task3_4, s, [1, 1000, start, 0])),
	register(s2, spawn(task3_4, s, [2, 1000, normal, 0])),
	register(s3, spawn(task3_4, s, [3, 1000, normal, 0])),
	register(s4, spawn(task3_4, s, [4, 1000, normal, 0])).
