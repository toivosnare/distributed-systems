-module(task5_4).
-export([count_yes_votes/2, manager/2, client/2, start/0]).

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

manager(Clients, Locks) ->
	receive
		{clients, NewClients} ->
			manager(NewClients, Locks);
		{lock_request, LockIndex, Client} ->
			{LockType, Holder, K} = maps:get(LockIndex, Locks),
			Response = case Holder of
				Client ->
					io:format("Client ~p requesting to lock ~p which it is already holding.~n", [Client, LockIndex]),
					not_ok;
				null ->
					io:format("Client ~p requesting to lock ~p which is not held by any client.~n", [Client, LockIndex]),
					ok;
				_ ->
					io:format("Client ~p requesting to lock ~p which is held by ~p.~n", [Client, LockIndex, Holder]),
					[C ! {vote_request, LockIndex} || C <- Clients, C /= Client],
					YesVotes = count_yes_votes(0, length(Clients) - 1),
					RequiredYesVotes = case LockType of
						write_lock -> K;
						read_lock -> length(Clients) - K + 1
					end,
					io:format("Manager received total of ~p yes vote(s) (~p vote(s) were required).~n", [YesVotes, RequiredYesVotes]),
					case YesVotes >= RequiredYesVotes of
						true -> ok;
						false -> not_ok
					end
			end,
			Client ! {lock_response, LockIndex, Response},
			case Response of
				ok -> manager(Clients, Locks#{LockIndex => {LockType, Client, K}});
				_ -> manager(Clients, Locks)
			end;
		{unlock_request, LockIndex, Client} ->
			{LockType, Holder, K} = maps:get(LockIndex, Locks),
			Response = case Holder of
				Client ->
					io:format("Client ~p requesting to unlock ~p which it is holding.~n", [Client, LockIndex]),
					ok;
				null ->
					io:format("Client ~p requesting to unlock ~p which is not held by any client.~n", [Client, LockIndex]),
					not_ok;
				_ ->
					io:format("Client ~p requesting to unlock ~p which is held by ~p.~n", [Client, LockIndex, Holder]),
					not_ok
			end,
			Client ! {unlock_response, LockIndex, Response},
			case Response of
				ok -> manager(Clients, Locks#{LockIndex => {LockType, null, K}});
				_ -> manager(Clients, Locks)
			end;
		quit ->
			io:format("Manager quitting.~n", [])
	end.

client(Manager, LockIndices) ->
	receive
		{lock_request, LockIndex} ->
			Manager ! {lock_request, LockIndex, self()},
			client(Manager, LockIndices);
		{lock_response, LockIndex, Response} ->
			io:format("Client ~p received lock response ~p for lock ~p.~n", [self(), Response, LockIndex]),
			case Response of
				ok -> client(Manager, [LockIndex | LockIndices]);
				_ -> client(Manager, LockIndices)
			end;
		{unlock_request, LockIndex} ->
			Manager ! {unlock_request, LockIndex, self()},
			client(Manager, LockIndices);
		{unlock_response, LockIndex, Response} ->
			io:format("Client ~p received unlock response ~p for lock ~p.~n", [self(), Response, LockIndex]),
			case Response of
				ok -> client(Manager, lists:delete(LockIndex, LockIndices));
				_ -> client(Manager, LockIndices)
			end;
		{vote_request, LockIndex} ->
			Vote = case lists:member(LockIndex, LockIndices) of
				false -> yes;
				true -> no
			end,
			Manager ! {vote_response, LockIndex, self(), Vote},
			client(Manager, LockIndices);
		quit ->
			io:format("Client ~p quitting.~n", [self()])
	end.

start() ->
	Locks = #{
		0 => {write_lock, null, 1},
		1 => {read_lock, null, 3}
	},
	Manager = spawn(task5_4, manager, [[], Locks]),
	Clients = [
		spawn(task5_4, client, [Manager, []]),
		spawn(task5_4, client, [Manager, []]),
		spawn(task5_4, client, [Manager, []])
	],
	Manager ! {clients, Clients},
	timer:sleep(100),
	lists:nth(1, Clients) ! {lock_request, 0},
	timer:sleep(100),
	lists:nth(2, Clients) ! {lock_request, 1},
	timer:sleep(100),
	lists:nth(3, Clients) ! {lock_request, 0},
	timer:sleep(100),
	lists:nth(1, Clients) ! {unlock_request, 0},
	timer:sleep(100),
	lists:nth(2, Clients) ! {unlock_request, 1},
	timer:sleep(100),
	lists:nth(3, Clients) ! {lock_request, 1},
	timer:sleep(100),
	lists:nth(1, Clients) ! {lock_request, 1},
	timer:sleep(100),
	lists:nth(2, Clients) ! {lock_request, 1},
	timer:sleep(100),
	lists:nth(3, Clients) ! {unlock_request, 1},
	timer:sleep(100),
	[C ! quit || C <- Clients],
	Manager ! quit.
