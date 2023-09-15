-module(task2_1).
-export([divider/0]).

on_exit(Pid, Fun) ->
	spawn(fun() ->
		process_flag(trap_exit, true),
		link(Pid),
		receive
			{'EXIT', Pid, Why} -> Fun(Why)
		end
	end).

keep_alive(Fun) ->
	Pid = spawn(Fun),
	on_exit(Pid, fun(_) -> keep_alive(Fun) end).

divider() ->
	keep_alive(fun() ->
		register(divider, self()),
		receive
			{Client, Msg} ->
				Result = case Msg of
					{add, Op1, Op2} -> Op1 + Op2;
					{subtract, Op1, Op2} -> Op1 - Op2;
					{multiply, Op1, Op2} -> Op1 * Op2;
					{divide, Op1, Op2} -> Op1 / Op2
				end,
				Client ! Result
		end
	end).
