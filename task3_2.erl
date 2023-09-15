-module(task3_2).
-export([function/1, send_message/4, receive_message/2, s1/1, s2/1, s3/1, s4/1, start/0]).

function(Process) ->
	case Process of
		s1 -> fun s1/1;
		s2 -> fun s2/1;
		s3 -> fun s3/1;
		s4 -> fun s4/1
	end.

send_message(Source, Destination, Message, {T1, T2, T3, T4}) ->
	NewTimestamp = case Source of
		s1 -> {T1 + 1, T2, T3, T4};
		s2 -> {T1, T2 + 1, T3, T4};
		s3 -> {T1, T2, T3 + 1, T4};
		s4 -> {T1, T2, T3, T4 + 1}
	end,
	io:format("Process ~p sending message ~p to process ~p with timestamp ~p.~n", [Source, Message, Destination, NewTimestamp]),
	Destination ! {Message, NewTimestamp},
	(function(Source))(NewTimestamp).


receive_message(Destination, {T1, T2, T3, T4}) ->
	receive
		{Message, MessageTimestamp} ->
			{MT1, MT2, MT3, MT4} = MessageTimestamp,
			NewTimestamp = {
				case Destination of s1 -> T1 + 1; _ -> case MT1 > T1 of true -> MT1; false -> T1 end end,
				case Destination of s2 -> T2 + 1; _ -> case MT2 > T2 of true -> MT2; false -> T2 end end,
				case Destination of s3 -> T3 + 1; _ -> case MT3 > T3 of true -> MT3; false -> T3 end end,
				case Destination of s4 -> T4 + 1; _ -> case MT4 > T4 of true -> MT4; false -> T4 end end
			},
			io:format("Process ~p received message ~p with timestamp ~p. New timetamp is ~p.~n", [Destination, Message, MessageTimestamp, NewTimestamp]),
			(function(Destination))(NewTimestamp)
	end.

s1(Timestamp) ->
	case Timestamp of
		{0, _, _, _} -> send_message(s1, s4, m1, Timestamp);
		{1, _, _, _} -> receive_message(s1, Timestamp);
		{2, _, _, _} -> send_message(s1, s4, m4, Timestamp);
		_ -> io:format("Process s1 exits.~n")
	end.

s2(Timestamp) ->
	case Timestamp of
		{_, 0, _, _} -> send_message(s2, s4, m2, Timestamp);
		{_, 1, _, _} -> receive_message(s2, Timestamp);
		{_, 2, _, _} -> send_message(s2, s4, m9, Timestamp);
		_ -> io:format("Process s2 exits.~n")
	end.

s3(Timestamp) ->
	case Timestamp of
		{_, _, 0, _} -> send_message(s3, s4, m5, Timestamp);
		{_, _, 1, _} -> receive_message(s3, Timestamp);
		{_, _, 2, _} -> send_message(s3, s4, m7, Timestamp);
		_ -> io:format("Process s3 exits.~n")
	end.

s4(Timestamp) ->
	case Timestamp of
		{_, _, _, 0} -> receive_message(s4, Timestamp);
		{_, _, _, 1} -> send_message(s4, s1, m3, Timestamp);
		{_, _, _, 2} -> receive_message(s4, Timestamp);
		{_, _, _, 3} -> receive_message(s4, Timestamp);
		{_, _, _, 4} -> receive_message(s4, Timestamp);
		{_, _, _, 5} -> send_message(s4, s3, m6, Timestamp);
		{_, _, _, 6} -> receive_message(s4, Timestamp);
		{_, _, _, 7} -> send_message(s4, s2, m8, Timestamp);
		{_, _, _, 8} -> receive_message(s4, Timestamp);
		_ -> io:format("Process s4 exits.~n")
	end.

start() ->
	register(s1, spawn(task3_2, s1, [{0, 0, 0, 0}])),
	register(s2, spawn(task3_2, s2, [{0, 0, 0, 0}])),
	register(s3, spawn(task3_2, s3, [{0, 0, 0, 0}])),
	register(s4, spawn(task3_2, s4, [{0, 0, 0, 0}])).
