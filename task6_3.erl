-module(task6_3).
-export([start1/0, start2/0, start3/0, start4/0, start5/0, start6/0]).

start1() ->
	Participants = [
		spawn(task6_1, participant, [yes, 0, 2000]),
		spawn(task6_1, participant, [yes, 0, 2000]),
		spawn(task6_1, participant, [yes, 0, 2000]),
		spawn(task6_1, participant, [yes, 0, 2000])
	],
	task6_2:coordinator(Participants, 1000, 1000).

start2() ->
	Participants = [
		spawn(task6_1, participant, [no, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000])
	],
	task6_2:coordinator(Participants, 1000, 1000).

start3() ->
	Participants = [
		spawn(task6_1, participant, [yes, 10000, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 1000])
	],
	task6_2:coordinator(Participants, 100, 1000).

start4() ->
	Participants = [
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0])
	],
	task6_1:coordinator(Participants, 1000).

start5() ->
	Participants = [
		spawn(task6_2, participant, [no, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0])
	],
	task6_1:coordinator(Participants, 1000).

start6() ->
	Participants = [
		spawn(task6_2, participant, [yes, 10000, 0]),
		spawn(task6_2, participant, [yes, 10000, 0]),
		spawn(task6_2, participant, [yes, 0, 0]),
		spawn(task6_2, participant, [yes, 0, 0])
	],
	task6_1:coordinator(Participants, 100).
