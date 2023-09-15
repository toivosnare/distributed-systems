-module(task2_2).
-export([client/1]).

client([Head | Tail]) ->
	io:format("~p = ", [Head]),
	divider ! {self(), Head},
	receive
		Result -> io:format("~p~n", [Result])
	after
		100 -> io:format("timed out~n", [])
	end,
	client(Tail);

client([]) -> ok.
