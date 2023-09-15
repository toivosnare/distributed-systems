-module(task5_1).
-export([participant/1, collect_votes/2, coordinator/1, start/0]).

participant(Vote) ->
	receive
		{vote_request, Sender} ->
			io:format("Participant sending vote: ~p.~n", [Vote]),
			Sender ! Vote,
			participant(Vote);
		{result, Result} ->
			io:format("Participant received result: ~p.~n", [Result])
	end.

collect_votes(Participants, Votes) when length(Participants) == length(Votes) -> commit;
collect_votes(Participants, Votes) ->
	receive Vote ->
		case Vote of
			no -> abort;
			yes -> collect_votes(Participants, [Vote | Votes])
		end
	end.

coordinator(Participants) ->
	[P ! {vote_request, self()} || P <- Participants],
	Result = collect_votes(Participants, []),
	[P ! {result, Result} || P <- Participants],
	ok.

start() ->
	Participants = [
		spawn(task5_1, participant, [yes]),
		spawn(task5_1, participant, [yes]),
		spawn(task5_1, participant, [no]),
		spawn(task5_1, participant, [yes])
	],
	coordinator(Participants).
