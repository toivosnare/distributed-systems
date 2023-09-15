-module(task7).
-export([server/3, start/0]).

server_failed(Servers, Subscriptions) ->
    receive
        recover ->
            io:format("Server ~p received recover request.~n", [self()]),
            [FirstServer | _] = Servers,
            FirstServer ! {get_data, self()},
            server(Servers, #{}, Subscriptions);
        _ ->
            server_failed(Servers, Subscriptions)
    end.

server(Servers, Data, Subscriptions) ->
    receive
        {servers, S} ->
            io:format("Server ~p received server list: ~p.~n", [self(), S]),
            server(S, Data, Subscriptions);
        {write, Key, Value} ->
            io:format("Server ~p received write request with K=~p, V=~p.~n", [self(), Key, Value]),
            [S ! {write_copy, Key, Value} || S <- Servers, S /= self()],
            Subscribers = maps:get(Key, Subscriptions, []),
            [P ! {value_updated, Key, Value} || P <- Subscribers],
            server(Servers, maps:put(Key, Value, Data), Subscriptions);
        {write_copy, Key, Value} ->
            io:format("Server ~p received write_copy request with K=~p, V=~p.~n", [self(), Key, Value]),
            Subscribers = maps:get(Key, Subscriptions, []),
            [P ! {value_updated, Key, Value} || P <- Subscribers],
            server(Servers, maps:put(Key, Value, Data), Subscriptions);
        {read, Key, Pid} ->
            io:format("Server ~p received read request from ~p with K=~p.~n", [self(), Pid, Key]),
            Value = maps:get(Key, Data, null),
            Pid ! {value, Key, Value},
            server(Servers, Data, Subscriptions);
        {subscribe, Key, Pid} ->
            io:format("Server ~p received subscribe request from ~p with K=~p.~n", [self(), Pid, Key]),
            Subscribers = maps:get(Key, Subscriptions, []),
            server(Servers, Data, maps:put(Key, [Pid | Subscribers], Subscriptions));
        fail ->
            io:format("Server ~p received fail request.~n", [self()]),
            server_failed(Servers, Subscriptions);
        {get_data, Pid} ->
            io:format("Server ~p received get_data request from ~p.~n", [self(), Pid]),
            maps:fold(fun(Key, Value, _) -> Pid ! {write_copy, Key, Value} end, ok, Data),
            server(Servers, Data, Subscriptions);
        quit ->
            io:format("Server ~p quitting.~n", [self()])
    end.

client(Servers) ->
    S1 = lists:nth(1, Servers),
    S2 = lists:nth(2, Servers),
    S1 ! {read, a, self()}, receive {value, K1, V1} -> io:format("~p => ~p~n", [K1, V1]) end,
    S1 ! {write, a, 0},
    S1 ! {read, a, self()}, receive {value, K2, V2} -> io:format("~p => ~p~n", [K2, V2]) end,
    S2 ! {read, a, self()}, receive {value, K3, V3} -> io:format("~p => ~p~n", [K3, V3]) end,
    S2 ! {subscribe, b, self()},
    S1 ! {write, b, 1}, receive {value_updated, K4, V4} -> io:format("~p => ~p~n", [K4, V4]) end,
    S2 ! fail,
    S1 ! {write, c, 2},
    S2 ! recover,
    timer:sleep(100),
    S2 ! {read, a, self()}, receive {value, K5, V5} -> io:format("~p => ~p~n", [K5, V5]) end,
    S2 ! {read, b, self()}, receive {value, K6, V6} -> io:format("~p => ~p~n", [K6, V6]) end,
    S2 ! {read, c, self()}, receive {value, K7, V7} -> io:format("~p => ~p~n", [K7, V7]) end.

start() ->
    Servers = [
        spawn(task7, server, [[], #{}, #{}]),
        spawn(task7, server, [[], #{}, #{}])
    ],
    [S ! {servers, Servers} || S <- Servers],
    client(Servers),
    [S ! quit || S <- Servers].
