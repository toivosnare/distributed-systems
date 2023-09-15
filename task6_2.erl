-module(task6_2).
-export([participant/3, coordinator/3, start/0]).

participant(Vote, VoteDelay, AcknowledgementDelay) ->
	receive
		vote_request ->
			io:format("Participant ~p sending vote: ~p in ~p ms.~n", [self(), Vote, VoteDelay]),
			task6_1:send_delayed(self(), vote_now, VoteDelay),
			participant(Vote, VoteDelay, AcknowledgementDelay);
		vote_now ->
			coordinator ! Vote,
			participant(Vote, VoteDelay, AcknowledgementDelay);
		prepare ->
			io:format("Participant ~p preparing in ~p ms.~n", [self(), AcknowledgementDelay]),
			task6_1:send_delayed(self(), acknowledge_now, AcknowledgementDelay),
			participant(Vote, VoteDelay, AcknowledgementDelay);
		acknowledge_now ->
			coordinator ! acknowledge,
			participant(Vote, VoteDelay, AcknowledgementDelay);
		abort -> io:format("Participant ~p aborting.~n", [self()]);
		commit -> io:format("Participant ~p commiting.~n", [self()])
	end.

coordinator(Participants, VoteTimeout, AcknowledgeTimeout) ->
	register(coordinator, self()),
	[P ! vote_request || P <- Participants],
	case task6_1:collect_n_messages(yes, length(Participants), VoteTimeout) of
		false -> [P ! abort || P <- Participants];
		true ->
			[P ! prepare || P <- Participants],
			Result = case task6_1:collect_n_messages(acknowledge, length(Participants), AcknowledgeTimeout) of
				false -> abort;
				true -> commit
			end,
			[P ! Result || P <- Participants]
	end,
	unregister(coordinator),
	ok.

start() ->
	Participants = [
		spawn(task6_2, participant, [yes, 100, 1400]),
		spawn(task6_2, participant, [yes, 200, 300]),
		spawn(task6_2, participant, [yes, 300, 200]),
		spawn(task6_2, participant, [yes, 400, 100])
	],
	coordinator(Participants, 1000, 1000).
