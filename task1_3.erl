-module(task1_3).
-export([start/0, rx/0, tx/1]).

rx() ->
	receive
		{TXPID, Date} ->
			TXPID ! task1_1:next_date(Date),
			rx();
		quit -> ok
	end.

tx([Head | Tail]) ->
	io:format("Date ~p is followed by ", [Head]),
	rx ! {self(), Head},
	receive
		Response -> io:format("~p.~n", [Response])
	end,
	tx(Tail);

tx([]) ->
	rx ! quit.

start() ->
    register(rx, spawn(task1_3, rx, [])),
	Dates = [{-1, 12, 31}, {400, 2, 28}, {2023, 8, 29}],
    spawn(task1_3, tx, [Dates]).
