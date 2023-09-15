-module(task4_4).
-export([server/0, try_operation/2, client/2, start/0]).

server() ->
	receive
		{Client, Msg} ->
			Result = case Msg of
				{add, Op1, Op2} -> Op1 + Op2;
				{subtract, Op1, Op2} -> Op1 - Op2;
				{multiply, Op1, Op2} -> Op1 * Op2;
				{divide, Op1, Op2} -> Op1 / Op2
			end,
			Client ! Result,
			server();
		fail ->
			io:format("Server ~p exiting.~n", [self()]),
			ok
	end.

try_operation([FirstServer | Rest], Operation) ->
	catch FirstServer ! {self(), Operation},
	receive Result ->
		io:format("Server ~p responded with ~p.~n", [FirstServer, Result]),
		Result
	after 1000 ->
		io:format("Server ~p timed out.~n", [FirstServer]),
		try_operation(Rest, Operation)
	end;
try_operation([], _) -> not_ok.

client(Servers, [FirstOperation | Rest]) ->
	io:format("Calculating ~p.~n", [FirstOperation]),
	try_operation(Servers, FirstOperation),
	client(Servers, Rest);
client(_, []) -> ok.

start() ->
	Servers = [spawn(fun server/0), spawn(fun server/0), spawn(fun server/0)],
	client(Servers, [{add, 1, 1}, {subtract, 2, 8}, {multiply, 8, 8}, {divide, 20, 5}]),
	[FirstServer | _] = Servers,
	FirstServer ! fail,
	client(Servers, [{add, 42, 0}, {subtract, 2, -8}, {multiply, 7, -9}, {divide, -5, 5}]).
