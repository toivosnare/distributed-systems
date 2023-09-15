-module(task5_3).
-export([count_yes_votes/2, process/2, start/0]).

count_yes_votes(YesVotes, MessagesLeft) when MessagesLeft == 0 -> YesVotes;
count_yes_votes(YesVotes, MessagesLeft) ->
	receive {vote_response, _, _, Vote} ->
		Y = case Vote of
			yes -> YesVotes + 1;
			_ -> YesVotes
		end,
		count_yes_votes(Y, MessagesLeft - 1)
	after 1000 -> YesVotes
	end.

process(Processes, Locks) ->
	receive
		{processes, NewProcesses} ->
			process(NewProcesses, Locks);
		{lock_request, LockIndex} ->
			{LockType, Holding, K} = maps:get(LockIndex, Locks),
			case Holding of
				false ->
					io:format("Process ~p requesting lock ~p.~n", [self(), LockIndex]),
					[P ! {vote_request, LockIndex, self()} || P <- Processes, P /= self()],
					YesVotes = count_yes_votes(0, length(Processes) - 1),
					RequiredYesVotes = case LockType of
						write_lock -> K;
						read_lock -> length(Processes) - K + 1
					end,
					io:format("Process ~p received total of ~p yes vote(s) (~p vote(s) were required).~n", [self(), YesVotes, RequiredYesVotes]),
					case YesVotes >= RequiredYesVotes of
						true ->
							[P ! {unlock, LockIndex} || P <- Processes, P /= self()],
							self() ! {lock, LockIndex};
						false -> ok
					end;
				true ->
					io:format("Process ~p requesting lock ~p that it already has.~n", [self(), LockIndex])
			end,
			process(Processes, Locks);
		{vote_request, LockIndex, Sender} ->
			{_, Holding, _} = maps:get(LockIndex, Locks),
			Vote = case Holding of
				false -> yes;
				true -> no
			end,
			io:format("Process ~p voting ~p for request for lock ~p from process ~p.~n", [self(), Vote, LockIndex, Sender]),
			Sender ! {vote_response, LockIndex, self(), Vote},
			process(Processes, Locks);
		{unlock, LockIndex} ->
			{LockType, Holding, K} = maps:get(LockIndex, Locks),
			case Holding of
				false ->
					process(Processes, Locks);
				true ->
					io:format("Process ~p unlocking lock ~p.~n", [self(), LockIndex]),
					process(Processes, Locks#{LockIndex := {LockType, false, K}})
			end;
		{lock, LockIndex} ->
			{LockType, Holding, K} = maps:get(LockIndex, Locks),
			case Holding of
				false ->
					io:format("Process ~p locking lock ~p.~n", [self(), LockIndex]),
					process(Processes, Locks#{LockIndex := {LockType, true, K}});
				true ->
					process(Processes, Locks)
			end;
		quit ->
			io:format("Process ~p quitting.~n", [self()])
	end.

start() ->
	Locks = #{
		0 => {write_lock, false, 1},
		1 => {read_lock, false, 3}
	},
	Processes = [
		spawn(task5_3, process, [[], Locks]),
		spawn(task5_3, process, [[], Locks]),
		spawn(task5_3, process, [[], Locks])
	],
	[P ! {processes, Processes} || P <- Processes],
	lists:nth(1, Processes) ! {lock, 0},
	lists:nth(2, Processes) ! {lock, 1},
	lists:nth(3, Processes) ! {lock_request, 0},
	timer:sleep(1000),
	lists:nth(1, Processes) ! {lock_request, 1},
	timer:sleep(1000),
	[P ! quit || P <- Processes].
