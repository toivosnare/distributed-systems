-module(task1_2).
-export([start/0, rx/0, tx/2]).

rx() ->
	receive
		{TXPID, Date} ->
			TXPID ! task1_1:next_date(Date),
			rx();
		quit -> ok
	end.

tx(RXPID, [Head | Tail]) ->
	io:format("Date ~p is followed by ", [Head]),
	RXPID ! {self(), Head},
	receive
		Response -> io:format("~p.~n", [Response])
	end,
	tx(RXPID, Tail);

tx(RXPID, []) ->
	RXPID ! quit.

start() ->
    RXPID = spawn(task1_2, rx, []),
	Dates = [{-1, 12, 31}, {400, 2, 28}, {2023, 8, 29}],
    spawn(task1_2, tx, [RXPID, Dates]).
