-module(task5_2).
-export([participant/1, collect_votes/2, coordinator/1, start/0]).

participant(Vote) ->
	receive
		{vote_request, Sender} ->
			if
				(Vote == no) or (Vote == yes) ->
					io:format("Participant sending vote: ~p.~n", [Vote]),
					Sender ! Vote,
					participant(Vote);
				true ->
					io:format("Participant not sending vote.~n", [])
			end;
		{result, Result} ->
			io:format("Participant received result: ~p.~n", [Result])
	end.

collect_votes(Participants, Votes) when length(Participants) == length(Votes) ->
	io:format("Received yes votes from all participants. Commiting.~n", []),
	commit;
collect_votes(Participants, Votes) ->
	receive Vote ->
		case Vote of
			no ->
				io:format("Received no vote. Aborting.~n", []),
				abort;
			yes -> collect_votes(Participants, [Vote | Votes])
		end
	after 1000 ->
		io:format("Voting timed out. Aborting.~n", []),
		abort
	end.

coordinator(Participants) ->
	[P ! {vote_request, self()} || P <- Participants],
	Result = collect_votes(Participants, []),
	[P ! {result, Result} || P <- Participants],
	ok.

start() ->
	Participants = [
		spawn(task5_2, participant, [yes]),
		spawn(task5_2, participant, [yes]),
		spawn(task5_2, participant, [none]),
		spawn(task5_2, participant, [yes])
	],
	coordinator(Participants).
