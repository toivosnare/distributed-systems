-module(task3_1).
-export([function/1, send_message/4, receive_message/2, s1/1, s2/1, s3/1, s4/1, start/0]).

function(Process) ->
	case Process of
		s1 -> fun s1/1;
		s2 -> fun s2/1;
		s3 -> fun s3/1;
		s4 -> fun s4/1
	end.

send_message(Source, Destination, Message, Timestamp) ->
	NewTimestamp = Timestamp + 1,
	io:format("Process ~p sending message ~p to process ~p with timestamp ~p.~n", [Source, Message, Destination, NewTimestamp]),
	Destination ! {Message, NewTimestamp},
	(function(Source))(NewTimestamp).

receive_message(Destination, Timestamp) ->
	receive
		{Message, MessageTimestamp} ->
			NewTimestamp = case MessageTimestamp > Timestamp of
				true -> MessageTimestamp + 1;
				false -> Timestamp + 1
			end,
			io:format("Process ~p received message ~p with timestamp ~p. New timetamp is ~p.~n", [Destination, Message, MessageTimestamp, NewTimestamp]),
			(function(Destination))(NewTimestamp)
	end.

s1(Timestamp) ->
	case Timestamp of
		0 -> send_message(s1, s4, m1, Timestamp);
		1 -> receive_message(s1, Timestamp);
		4 -> send_message(s1, s4, m4, Timestamp);
		_ -> io:format("Process s1 exits.~n")
	end.

s2(Timestamp) ->
	case Timestamp of
		0 -> send_message(s2, s4, m2, Timestamp);
		1 -> receive_message(s2, Timestamp);
		12 -> send_message(s2, s4, m9, Timestamp);
		_ -> io:format("Process s2 exits.~n")
	end.

s3(Timestamp) ->
	case Timestamp of
		0 -> send_message(s3, s4, m5, Timestamp);
		1 -> receive_message(s3, Timestamp);
		8 -> send_message(s3, s4, m7, Timestamp);
		_ -> io:format("Process s3 exits.~n")
	end.

s4(Timestamp) ->
	case Timestamp of
		0 -> receive_message(s4, Timestamp);
		2 -> send_message(s4, s1, m3, Timestamp);
		3 -> receive_message(s4, Timestamp);
		4 -> receive_message(s4, Timestamp);
		5 -> receive_message(s4, Timestamp);
		6 -> send_message(s4, s3, m6, Timestamp);
		7 -> receive_message(s4, Timestamp);
		10 -> send_message(s4, s2, m8, Timestamp);
		11 -> receive_message(s4, Timestamp);
		_ -> io:format("Process s4 exits.~n")
	end.

start() ->
	register(s1, spawn(task3_1, s1, [0])),
	register(s2, spawn(task3_1, s2, [0])),
	register(s3, spawn(task3_1, s3, [0])),
	register(s4, spawn(task3_1, s4, [0])).
