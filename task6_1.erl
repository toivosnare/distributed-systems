-module(task6_1).
-export([send_delayed/3, collect_n_messages/3, participant/3, coordinator/2, start/0]).

send_delayed(Recipient, Message, Delay) ->
	spawn(fun() -> timer:sleep(Delay), Recipient ! Message end).

collect_n_messages(ExpectedMessage, N, _, MessagesCollected) when N == MessagesCollected ->
	io:format("Received ~p message from all participants.~n", [ExpectedMessage]),
	true;
collect_n_messages(ExpectedMessage, N, Timeout, MessagesCollected) ->
	receive Message ->
		case Message of
			ExpectedMessage -> collect_n_messages(ExpectedMessage, N, Timeout, MessagesCollected + 1);
			_ ->
				io:format("Received unexpected message: ~p.~n", [Message]),
				false
		end
	after Timeout ->
		io:format("Timed out waiting for ~p message from all participants.~n", [ExpectedMessage]),
		false
	end.
collect_n_messages(ExpectedMessage, N, Timeout) ->
	collect_n_messages(ExpectedMessage, N, Timeout, 0).

participant(Vote, VoteDelay, CancelTimeout) ->
	receive
		vote_request ->
			io:format("Participant ~p sending vote: ~p in ~p ms.~n", [self(), Vote, VoteDelay]),
			send_delayed(self(), vote_now, VoteDelay),
			participant(Vote, VoteDelay, CancelTimeout);
		vote_now ->
			coordinator ! Vote,
			send_delayed(self(), cancel, CancelTimeout),
			participant(Vote, VoteDelay, CancelTimeout);
		cancel ->
			io:format("Participant ~p timed out waiting for result. Sending cancel.~n", [self()]),
			coordinator ! cancel,
			participant(Vote, VoteDelay, CancelTimeout);
		abort -> io:format("Participant ~p aborting.~n", [self()]);
		commit -> io:format("Participant ~p commiting.~n", [self()])
	end.

coordinator(Participants, VoteTimeout) ->
	register(coordinator, self()),
	[P ! vote_request || P <- Participants],
	Result = case collect_n_messages(yes, length(Participants), VoteTimeout) of
		false -> abort;
		true -> commit
	end,
	[P ! Result || P <- Participants],
	unregister(coordinator),
	ok.

start() ->
	Participants = [
		spawn(task6_1, participant, [yes, 0, 1000]),
		spawn(task6_1, participant, [yes, 0, 3000]),
		spawn(task6_1, participant, [yes, 0, 3000]),
		spawn(task6_1, participant, [yes, 2000, 3000])
	],
	coordinator(Participants, 5000).
