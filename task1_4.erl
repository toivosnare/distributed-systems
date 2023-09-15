-module(task1_4).
-export([start/0, rx/0, tx/1]).

rx() ->
	receive
		{TXPID, Date} ->
			TXPID ! task1_1:next_date(Date),
			rx();
		quit -> ok
	end.

tx([Head | Tail]) ->
	io:format("Asking which date ~p is followed by from rx0.~n", [Head]),
	rx0 ! {self(), Head},
	receive
		Response0 -> io:format("Answer from rx0: ~p.~n", [Response0])
	end,
	io:format("Asking which date ~p is followed by from rx1.~n", [Head]),
	rx1 ! {self(), Head},
	receive
		Response1 -> io:format("Answer from rx1: ~p.~n", [Response1])
	end,
	tx(Tail);

tx([]) ->
	rx0 ! quit,
	rx1 ! quit.

start() ->
    register(rx0, spawn(task1_4, rx, [])),
    register(rx1, spawn(task1_4, rx, [])),
	Dates = [{-1, 12, 31}, {400, 2, 28}, {2023, 8, 29}],
    spawn(task1_4, tx, [Dates]),
    spawn(task1_4, tx, [Dates]).
