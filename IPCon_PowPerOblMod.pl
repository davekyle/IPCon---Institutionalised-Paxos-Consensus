/****************************************************************
 *                                                              *
 * Institutionalised Paxos Consensus (IPCon)					*
 * David Sanderson & Jeremy Pitt, Imperial College London		*
 * Event Calculus by Marek Sergot								*
 * Original chaired Floor Control Protocol by Alexander Artikis * 
 *                                                              *
 *                                                              *
 * Implemented in YAP (ISO-compliant mode) / SWI Prolog         *
 ****************************************************************/
/*:- set_flag(all_dynamic, on).
*/
:- use_module(library(lists)). 
:- dynamic
    happens/2.
	
/************************* 
 * EVENT CALCULUS AXIOMS *
 *************************/
holdsAt(Fluent, T) :-
	initially(Fluent),
	\+ broken(Fluent, 0, T).
holdsAt(Fluent, T) :-
	happens(Event, EarlyTime),
	EarlyTime < T,
	initiates(Event, Fluent, EarlyTime),
	\+ broken(Fluent, EarlyTime, T).
broken(Fluent, T1, T3) :-
	happens(Event, T2),
	T1 =< T2,
	T2 < T3,
	terminates(Event, Fluent, T2).
terminates(Event, Fluent = V, T) :-
	initiates(Event, Fluent = V2, T),
	\+ (V = V2).

/****************************************
 * SYNTAX OF ACTIONS                    *
 *                                      *
 * request0a(Agent, Value, Issue, Cluster)         *
 * prepare1a(Agent, Ballot, Issue, Cluster)        *
 * respond1b(Agent1, (Agent2, PreviousBallot, Value), Ballot, Issue, Cluster)      		*
 * submit2a(Agent, Ballot, Value, Issue, Cluster) 		*
 * vote2b(Agent, Ballot, Value, Issue, Cluster)           *
 * arrogate(Agent, Issue, Cluster)          *
 * resign(Agent, Issue, Cluster)      	*
 * etc									*
 ***************************************/                          



% ----- initially no agent is sanctioned

initially( sanction(Agent) = [] ) :-
	setOfAllAgents(Agents),
	member(Agent, Agents).
% ----- otherwise initially every other boolean valued fluent is false
% ----- the value of 'pow', 'per', `obl' and sanction is determined by state constraints                     
initially( Fluent = false ) :-
	\+ ( Fluent = proposed(_,_,_) ),
	\+ ( Fluent = pre_vote(_,_,_) ),
	\+ ( Fluent = hnb(_,_,_) ),
	\+ ( Fluent = open_vote(_,_,_,_) ),
	\+ ( Fluent = safe_at(_,_,_,_,_) ),
	\+ ( Fluent = pow(_, _) ),
	\+ ( Fluent = per(_, _) ),
	\+ ( Fluent = voted(_,_,_,_,_) ),
	\+ ( Fluent = role_of(_,_,_,_) ),
	\+ ( Fluent = reportedVote(_,_,_,_,_,_ ) ),
	
	\+ ( Fluent = sync(_, _, _, _, _) ),
	
	\+ ( Fluent = quorum_size( _, _, _) ),
	
	\+ ( Fluent = role_of(_,_,_) ),
	\+ initially( Fluent = true ).
	
	
% proposer can propose if they have the pwr
initiates(request0a( P, V, I, C ),(proposed( V, I, C ) = true), T) :- %write('Req?'), nl,
	holdsAt((pow( P, request0a(P,V,I,C) ) = true), T). %, write('Req!'), nl.
% proposer has pwr if they're a proposer
holdsAt((pow( P, request0a(P,V,I,C) ) = true), T) :- %write('ReqPow?'), nl,
	holdsAt((role_of( P, proposer, I, C ) = true),T). %, write('ReqPow!'),nl.
% this should always return true, because you want anyone (even external agents?) to be able to check values
holdsAt((per( P, request0a(P,V,I,C) ) = true), T) :-
	holdsAt((pow( P, request0a(P,V,I,C) ) = true), T).
%%
%% Optional 0b/0c msg where leader informs proposer of results of response1b/vote2b respectively
%%

% leader can send prepare1a to start preVote with # B on I in C if they have the pwr. Creates an empty list for history.
initiates(prepare1a( L, B, I, C ),(pre_vote( B, I, C ) = true),T) :-
	B = ( R, BB ),
	%holdsAt((per( L, prepare1a(L,B,I,C) ) = true),T),
	holdsAt((pow( L, prepare1a(L,B,I,C) ) = true),T).
%% prepare terminates any previous pre_votes on the same issue
terminates(prepare1a( L, B, I, C ),(pre_vote( (ZR,ZB), I, C ) = true),T) :-
	B = (BR,BB),
	ZR =< BR,
	ZB < BB,
	%holdsAt((per( L, prepare1a(L,B,I,C) ) = true),T),
	holdsAt((pow( L, prepare1a(L,B,I,C) ) = true),T).
% leader has pwr to prepare1a if they're the leader
holdsAt((pow( L, prepare1a(L,B,I,C) ) = true),T) :-
	holdsAt((role_of( L, leader, I, C ) = true),T).
holdsAt((per( L, prepare1a(L,B,I,C) ) = true),T) :-
	holdsAt((pow( L, prepare1a(L,B,I,C) ) = true),T).
holdsAt((obl( L, prepare1a(L,B,I,C) ) = true),T) :-
	%holdsAt((pow( L, prepare1a(L,B,I,C) ) = true),T), %per includes pow
	holdsAt((per( L, prepare1a(L,B,I,C) ) = true),T),
	holdsAt((proposed( V, I, C ) = true), T),
	% FIXME TODO this should be (R,_) ? needs to be in the same revision...
	\+ holdsAt((pre_vote( (ZR,ZB), I, C ) = true),T).


% learners and acceptors can send response1b msgs informing on others for addition to the hnb list if they have pwr and permission
%initiates(response1b( LA, (A,N,V), B, I, C ), (hnb(B,I,C) = [ ( A, N, V ) | Q ]), T) :-
%% FIXME should be like this...
% CHANGED to allow only acceptors - learner interaction removed to reduce confusion
initiates(response1b( A, (A,N,V), B, I, C ), (reportedVote(A,N,V,B,I,C ) = true), T) :-
	N = ( R_N, BB_N ),
	B = ( R, BB ),
	%holdsAt((hnb(B,I,C) = [Q]), T),
	%write('assigned'), nl,
	holdsAt((pow( A, response1b(A,(A,N,V),B,I,C) ) = true), T). %, write('pow\'d'), nl.
	%holdsAt((per( LA, response1b(LA,(A,N,V),B,I,C) ) = true), T). %, write('REPORTED').
	% ( or can send a didntVote msg somehow )
% learner/acceptor has pwr to response1b if theyre a learner/acceptor
holdsAt((pow( A, response1b(A,(A,N,V),B,I,C) ) = true), T) :-
	B = ( R, BB ),
	holdsAt(( role_of( A, acceptor, R, I, C ) = true), T),
	holdsAt((pre_vote(B,I,C) = true), T).
% acceptor has permission to response1b if they're replying to the leader and they did actually vote previously
holdsAt((per( A, response1b(A,(A,N,V),B,I,C) ) = true), T) :-
	%holdsAt((pre_vote(B,I,C) = true), T),
	holdsAt((pow( A, response1b(A,(A,N,V),B,I,C) ) = true), T),
	holdsAt((voted(A,N,V,I,C ) = true), T).
holdsAt((per( A, response1b(A,(A,N,V),B,I,C) ) = true), T) :-
	B = (BR, BB),
	%holdsAt((pre_vote(B,I,C) = true), T),
	holdsAt((pow( A, response1b(A,(A,N,V),B,I,C) ) = true), T),
	\+ holdsAt((voted(A,Val,(BR,Ball),I,C ) = true), T),
	not(Val = null), Ball > BB,
	N = (BR,0), V = null.
% acceptor obligated to respond if they voted and havent responded already
% should add - "or voted in something higher"
holdsAt((obl( A, response1b(A,(A,N,V),B,I,C) ) = true), T) :-
	holdsAt((per( A, response1b(A,(A,N,V),B,I,C) ) = true), T),
	holdsAt((voted(A,N,V,I,C ) = true), T),
	\+ holdsAt((reportedVote(A,N,V,B,I,C ) = true), T).

% leader can submit2a and start the ballot if they have pwr and permission.
initiates(submit2a( L, B, V, I, C ), (open_vote( B, V, I, C ) = true), T) :- %write('sub?'), nl,
	B = ( R, BB ), %write('B'), nl,
	holdsAt((pow( L, submit2a(L,B,V,I,C) ) = true), T). %, %write('subPow'), nl,
	%holdsAt((per( L, submit2a(L,B,V,I,C) ) = true), T). %, write('subPer'), nl.
% leader has pwr to submit2a if theyre the leader
holdsAt((pow( L, submit2a(L,B,V,I,C) ) = true), T) :-
	holdsAt((role_of(L, leader, I, C ) = true), T), %write('isLeader'),nl.
	holdsAt((pre_vote( B, I, C ) = true), T).  
% leader has permission to submit2a if V has been proposed, they got a quorum response of response1b msgs, and V is safe at B
% holdsAt((per( L, submit2a(L,B,V,I,C) ) = true), T) :-
	% B = ( R, BB ),
	% holdsAt((proposed( V, I, C ) = true), T),
	%Q_ = subset( hnb(B,I,C) ),
	% subset( hnb(B,I,C), Q_ ), %%FIXME hnb IS THE PROBLEM ! %%
	% this allows the leader to choose any quorum-size group of unique responses to work with
	% holdsAt((length( Q_, X)), T),
	% holdsAt((quorum_size( I, C, Y)), T),
	% X >= Y,
	% holdsAt((safe_at( V, Q_, B, I, C ) = true), T).
% ( need to tidy up proposed() after value is chosen ? but not before it is chosen ! might need >1 ballot ... what about open_vote() ?)

%% submit shouldn't remove pre_votes - more submits may need to happen after the first one...

holdsAt((per( L, submit2a(L,B,V,I,C) ) = true), T) :- %write('subPer?'), nl,
	%write('Calculating permission to submit2a('),write(L),write(','),write(B),write(','),write(V),write(','),write(I),write(','),write(C),write(')'),nl,
	B = ( R, BB ),
	holdsAt((pow( L, submit2a(L,B,V,I,C) ) = true), T),
	holdsAt((proposed( V, I, C ) = true), T), % this line only lets you submit proposed values. comment out this line to unbind what you can submit.
	%holdsAt((pre_vote( B, I, C ) = true), T), % this is in pow) % this line tells you what B=(R,BB) to look for safety at
	%quorum_size( R, I, C, Size), 
	holdsAt((quorum_size( R, I, C)= Size),T),
	%% FIXME (Gave up on hnb and doing it on the fly :P)
	%% Get all votes. Should be reportedVote() FIXME - also fix case where there are no previous votes%%
	setof((Agent,RevBall,Value), holdsAt( (reportedVote(Agent,RevBall,Value,B,I,C ) = true), T ), AllVotes1), %write('gotVotes'), nl,
	%write('AllVotes:'), write(AllVotes1), nl,
	%setof((Agent,RevBall,Value), I^C^holdsAt( voted( Agent, RevBall, Value, I, C ) = true, T ), AllVotes1),
	%% Get set of agents that have had a reply about them - need at least Quorumsize to start. %%
	findall( (Agent) , member((Agent,RevBall,Value),AllVotes1), ReportedAgents), %write('gotAgents'), nl,
	%write('Reported Agents:'), write(ReportedAgents), write(' // '),
	length(ReportedAgents, NumberOfAgents), NumberOfAgents >= Size,  %write('gotNumber'), nl,
	%write(ReportedAgents), nl,
	clean(ReportedAgents, AllVotes1, AllVotes), %write('cleaned'), nl,
	%cleanT(AllVotes1, T, AllVotes), 
	%write('Cleaned:'), write(AllVotes), nl,
	%% - %%
	%% Check there are no votes in revisions later than this one %%
	max_in(AllVotes, MaxVote), %write('Got all'), nl,
	getxr(MaxVote, MaxVoteR), MaxVoteR =< R, %write('R:'), write(R), write(' // MaxR:'), write(MaxVoteR), nl,
	%% need to not check B if MaxR<R
	( MaxVoteR = R -> 	getxb(MaxVote, MaxVoteB), %write('HERE'), nl, 
						MaxVoteB < BB ; true %write('DO NOTHING'), nl %
	),
	%write('No votes in later ballots'), nl,
	%% - %%
	%write('V1:'), write(V), nl,
	%% Need to remove all the instances of previous votes per agent, so that only each agent's hnb is in the list %%
	%% clean AllVotes of all but highest per agent
	%% Bail if there are quorumsize or more votes in this revision and V!=Vote_V (findall, so that emptyList is returned rather than failure)%%
	findall( (Agent,(R,RBall),Value) , (member((Agent,(R,RBall),Value),AllVotes),(not(Value = V)),(not(Value = null))), ThisRevVotes), 
	findall( (Agent) , (member((Agent,(R,RBall),Value),AllVotes),(not(Value = V)),(not(Value = null))), ThisRevAgents), 
	%findall( (Agent,(R,RBall),Value) , (R^RBall^I^C^holdsAt( (voted( Agent, (R,RBall), Value, I, C ) = true), T ),(not(Value = V)),(not(Value = null)) ), ThisRevVotes), 
	%write('ThisRevVotes'), write(ThisRevVotes), nl,
	clean(ThisRevAgents, ThisRevVotes, CleanThisRevVotes),
	%write('CleanThisRevVotes:'), write(CleanThisRevVotes), nl,
	length(CleanThisRevVotes, Length), %write('Length:'), write(Length), nl,
	Length < Size, %write('1'), nl,
	%% - %%
	%write(Votes), nl,
	%write('Size:'), write(Size), nl,
	%% Get all the possible quorum-size groups, then test in order to 
	%write('V2:'), write(V), nl,
	setof(S, subsetN( AllVotes, [], Size, S), Result), member(Member, Result),
	%write('Testing Member:'), write(Member),nl,
	max_in(Member, MemberMax), %write('Max:'), write(MemberMax), nl,
	getxv(MemberMax, MMVal), %write('MMVal:'), write(MMVal), nl,
	%write('Checking for :'), write(MMVal), write(' / '), write(V), nl,
	(MMVal = V ; MMVal = null),  %write('Member Quorum: '), write(Member),  nl,%nl.
	%write('Per to submit '), write(V), write(' in '), write(B),nl, 
	!. %% Putting this in removes a million duplicate permissions/votes, BUT it also means permission to submit shows up once even if you have it for more than one


holdsAt((obl( L, submit2a(L,B,V,I,C) ) = true), T) :-
	holdsAt((per( L, submit2a(L,B,V,I,C) ) = true), T),
	\+ holdsAt((open_vote( B, V, I, C ) = true), T).
	

% acceptor can vote2b to vote if they have the pwr and permission
initiates(vote2b( A, B, V, I, C ), (voted( A, B, V, I, C ) = true), T) :-
	B = ( R, BB ),
	%write('initR:'), write(R), nl,
	holdsAt((pow( A, vote2b(A,B,V,I,C) ) = true), T). %,
	%holdsAt((per( A, vote2b(A,B,V,I,C) ) = true), T).
initiates(vote2b( A, B, V, I, C ), (reportedVote( A, B, V, B, I, C ) = true), T) :-
	B = ( R, BB ),
	%write('initR:'), write(R), nl,
	holdsAt((pow( A, vote2b(A,B,V,I,C) ) = true), T). %,
	%holdsAt((per( A, vote2b(A,B,V,I,C) ) = true), T).
% (the voted() has to include the value, so does that mean the vote2b() has to as well?)
% acceptor has power if they're acceptor
holdsAt((pow( A, vote2b(A,B,V,I,C) ) = true), T) :-
	B = ( R, BB ),
	%write('powR'), write(R),nl,
	holdsAt((role_of( A, acceptor, R, I, C ) = true), T),
	holdsAt((open_vote(B,V,I,C) = true), T).
% acceptor has permission to vote2b if they're replying to the leader and didn't already vote in a higher ballot
holdsAt((per( A, vote2b(A,B,V,I,C) ) = true), T) :-
	B = ( R, BB ),
	holdsAt((pow( A, vote2b(A,B,V,I,C) ) = true), T),
	%write(V), write(' in '), write(B), write(' for '), write(A), nl,
	%write('perR:'), write(R), write(' // BB:'), write(BB),nl,
	%write('B:'), write(B), write(' // V:'), write(V), nl,
	%holdsAt((open_vote(B,V,I,C) = true), T), %in pow
	%write('open?'),
	%%FIXME%%
	%holdsAt(({ forAll X>B, voted(A,X,_,I,C) = false }), T).
	%find the set of voted() where X>B. If there aren't any then go.
	(	findall((XR,XB),(Value^holdsAt( voted(A,(XR,XB),Value,I,C) = true, T)), Set),
		%append([], Set, String),
		%write('Set:'),write(Set), nl,
		%write('R:'), write(R), write(' // BB:'), write(BB),nl,
		didntVoteAbove(R,BB,Set) %, ! % removing this ! causes duplicate votes to be registered, but removes votePermissions from display
		%write('yes?').
		; 
		%% or if they didn't vote
		( \+ holdsAt((voted(A,(R,_),_,I,C ) = true), T) )%, write('Didn\'t vote'),nl  
	). %, write(A), write(' hasPer to vote '), write(V), write(' in '), write(BB), nl.
% ( in which case they should NACK to inform Leader - this means that the leader's msg was delayed, or there are 2 leaders )

didntVoteAbove(R,BB,Set) :-
	%write('Set:'), write(Set), nl,
	max_in(Set, (XR,XB)),
	%write('XR:'), write(XR), write(' // XB:'), write(XB),nl,
	%Result = (XR, XB),
	%write('R:'), write(R), write(' // BB:'), write(BB),nl,
	%write('XR:'), write(XR), write(' // XB:'), write(XB),nl,
	XR =< R,
	XB < BB.
	
% any agent can arrogate leadership at any time. ( they might lose it later )
initiates(arrogateLeadership( L, I, C ),(role_of( L, leader, any, I, C ) = true),T) :-
	\+ holdsAt((role_of(L, leader, R, I, C ) = true),T).
% permission to arrogate if no leader already
holdsAt(per(arrogateLeadership( L, I, C )) = true, T) :-
	\+ holdsAt((role_of(_, leader, R, I, C ) = true),T).
% agent can resign a role if it is holds it
terminates(resignLeadership( L, I, C ),(role_of( L, leader, _, I, C ) = true), T) :-
	holdsAt(pow(resignLeadership( L, I, C )) = true, T).
holdsAt(pow(resignLeadership( L, I, C )) = true, T) :-
	holdsAt((role_of(L, leader, _, I, C ) = true),T).

% agent can leave a cluster whenever they like. NOTE list-based leaving breaks obl so removed.
terminates(leaveCluster( A, C ),(role_of( A, _, _, _, C ) = true), T).
terminates(leaveCluster( A, C ),(role_of( A, _, _, C ) = true), T).
% terminates(leaveCluster( List, C ),(role_of( A, _, _, _, C ) = true), T) :-
	% member(A, List).
% terminates(leaveCluster( List, C ),(role_of( A, _, _, C ) = true), T) :-
	% member(A, List).
	
terminates(leaveCluster( A, C ), (reportedVote( A, _, _, _, _, C ) = true), T) :-
	holdsAt((reportedVote( A, _, _, _, _, C ) = true), T).
% terminates(leaveCluster( List, C ), (reportedVote( A, _, _, _, _, C ) = true), T) :-
	% member(A, List),
	% holdsAt((reportedVote( A, _, _, _, _, C ) = true), T).
	
% note, can't do multiple leaveCluster in one timestamp if this is to work.
initiates(leaveCluster( A, C ), obl(L, revise(L, I, C)) = true , T) :-
	holdsAt((role_of(A, acceptor, R, I, C ) = true),T), %write('Removed an acc'), nl,
	holdsAt(per(L, revise( L, I, C)) = true, T),%write('Permission to revise'), nl,
	% FIXME TODO these B's might be wrong... agent needs to have voted for the value but might not have reported at time of voting
	holdsAt((reportedVote( A, B, V, B, I, C ) = true), T),
	holdsAt(possibleRemRevision(V, R, I, C) = true, T). %, write('REVISE NOW'), nl.
% initiates(leaveCluster( List, C ), obl(L, revise(L, I, C)) = true , T) :-
	% member(A, List),
	% holdsAt((role_of(A, acceptor, R, I, C ) = true),T), %write('Removed an acc'), nl,
	% holdsAt(per(L, revise( L, I, C)) = true, T),%write('Permission to revise'), nl,
	% holdsAt((reportedVote( A, B, V, B, I, C ) = true), T),
	% holdsAt(possibleRemRevision(V, R, I, C) = true, T). %, write('REVISE NOW'), nl.

% leader can add anyone to any role whenever they want, unless that person already has the role.
initiates(addRole( L, A, Role, R, I, C ), (role_of( A, Role, R, I, C ) = true), T) :-
	holdsAt(pow(L, addRole( L, A, Role, R, I, C )) = true, T). %,
	%holdsAt(per(L, addRole( L, A, Role, R, I, C )) = true, T).
	
% add multiple agents at once
initiates(addRole( L, List, Role, R, I, C), (role_of( A, Role, R, I, C) = true), T) :-
	member(A, List),
	holdsAt(pow(L, addRole( L, A, Role, R, I, C )) = true, T). %,
	%holdsAt(per(L, addRole( L, A, Role, R, I, C )) = true, T).
	
holdsAt(pow(L, addRole( L, A, Role, R, I, C )) = true, T) :-
	holdsAt((role_of(L, leader, _, I, C ) = true),T),
	\+ holdsAt((role_of( A, Role, R, I, C ) = true),T).
holdsAt(per(L, addRole( L, A, Role, R, I, C )) = true, T) :-
	holdsAt(pow(L, addRole( L, A, Role, R, I, C )) = true, T).
	%\+ holdsAt((role_of( A, Role, R, I, C ) = true),T).
	
% leader can remove anyone's role whenever they want, provided that person already has the role.
terminates(remRole( L, A, Role, R, I, C ), (role_of( A, Role, R, I, C ) = true), T) :-
	holdsAt(pow(L, remRole( L, A, Role, R, I, C )) = true, T). %,
	%holdsAt(per(L, remRole( L, A, Role, R, I, C )) = true, T).
terminates(remRole( L, A, Role, R, I, C ), (reportedVote( A, (R, _), _, (R,_), I, C ) = true), T) :-
	holdsAt(pow(L, remRole( L, A, Role, R, I, C )) = true, T),
	holdsAt((reportedVote( A, (R, _), _, (R,_), I, C ) = true), T).
	
holdsAt(pow(L, remRole( L, A, Role, R, I, C )) = true, T) :-
	holdsAt((role_of(L, leader, _, I, C ) = true),T),
	holdsAt((role_of( A, Role, R, I, C ) = true),T).
holdsAt(per(L, remRole( L, A, Role, R, I, C )) = true, T) :-
	holdsAt(pow(L, remRole( L, A, Role, R, I, C )) = true, T).
	%holdsAt((role_of( A, Role, R, I, C ) = true),T).
	
% note, can't do multiple leaveCluster in one timestamp if this is to work.
initiates(remRole( L, A, acceptor, R, I, C ), obl(L, revise(L, I, C)) = true , T) :-
	holdsAt(per(L, remRole( L, A, acceptor, R, I, C )) = true, T), %write('Removed an acc'), nl,
	holdsAt(per(L, revise( L, I, C)) = true, T),%write('Permission to revise'), nl,
	% FIXME TODO these B's might be wrong... agent needs to have voted for the value but might not have reported at time of voting
	holdsAt((reportedVote( A, B, V, B, I, C ) = true), T),
	holdsAt(possibleRemRevision(V, R, I, C) = true, T). %, write('REVISE NOW'), nl.
	
	
% convenience action for making a new revision
initiates(revise( L, I, C), (role_of(Ag, acceptor, NewR, I, C) = true), T) :-
	holdsAt(pow(L, revise( L, I, C)) = true, T),
	%holdsAt(per(L, revise( L, I, C)) = true, T),
	% find all all agents that are acceptors in this issue/cluster
	findall((A,(R,0),v), R^holdsAt((role_of(A, acceptor, R, I, C) = true), T), Agents),
	% find the highest revision that any is acceptor in
	max_in(Agents, MaxA), MaxA = (_, (MaxR, _), _),
	% find all the agents that are acceptor in that revision
	member(Agent, Agents), getxr(Agent,MaxR), Agent = (Ag, _, _),
	% that make them acceptors in your new revision
	NewR is MaxR + 1.

% Revisions terminate all sync because the ppl being synced will be in the revision.
terminates(revise( L, I, C), sync(_, _, _, I, C) = true, T) :-
	holdsAt(pow(L, revise( L, I, C)) = true, T). %,
	%holdsAt(per(L, revise( L, I, C)) = true, T).
	
terminates(revise( L, I, C), obl(L, revise(L, I, C)) = true, T) :-
	holdsAt(pow(L, revise( L, I, C)) = true, T). %,
	%holdsAt(per(L, revise( L, I, C)) = true, T).
	
holdsAt(pow(L, revise( L, I, C)) = true, T) :-
	% you can do it if youre the leader
	holdsAt((role_of(L, leader, _, I, C ) = true),T).
holdsAt(per(L, revise( L, I, C)) = true, T) :-
	holdsAt(pow(L, revise( L, I, C)) = true, T).
	
%% obligation to revise when chosenness is broken.
% holdsAt(obl(L, revise(L, I, C)) = true, T) :-
	% holdsAt(per(L, revise( L, I, C)) = true, T),
	% T1 is T-1,
	% chosen(V, R, I, C, T1),
	% \+ chosen(V, R, I, C, T). %,
	%write('Chosen - '), write(V), write(','), write(R), write(','), write(I), write(','), write(C), write(','), write(T), write('('), write(T1), write(')'), nl.

	
% new acceptor that changes quorumsize (QS is even after join) initiates a synchronisation
initiates(syncReq(L, A, V, R, I, C), sync(A, V, R, I, C) = true, T) :- %write('SyncReq'), nl,
	holdsAt(pow(L, syncReq(L, A, V, R, I, C)) = true, T). %, %write('powSynReq'), nl,
	%holdsAt(per(L, syncReq(L, A, V, R, I, C)) = true, T). %, write('perSynReq').
	
holdsAt(pow(L, syncReq(L, A, V, R, I, C)) = true, T) :-
	holdsAt((role_of(L, leader, _, I, C ) = true),T).
	
holdsAt(per(L, syncReq(L, A, V, R, I, C)) = true, T) :-
	holdsAt(pow(L, syncReq(L, A, V, R, I, C)) = true, T),
	%write('In per with T:'), write(T), nl,
	% setOfHighestVotes(I, C, T, AllVotes), 
	% max_in(AllVotes, Max),
	% getxr(Max, R),
	% getxv(Max, V),
	%% should only send a syncReq if a value has been chosen in the previous time
	%% (additional agents changing quorum means it might not remain "chosen" due to them)
	%write('V:'), write(V), write(' R:'), write(R), write(' I:'), write(I), write(' C:'), write(C), write(' T:'), write(T), nl,
	chosen(V, R, I, C, T-1), %write('chosen'), nl,
	% an even quorumsize after they join means that the quorumsize just changed
	%% (don't want this in the case of lots of agents joining at once)
	%holdsAt((quorum_size( R, I, C) = Size),T), %write('got Size'),nl,
	%0 is Size mod 2, %write('even'),nl,
	holdsAt((role_of(A, acceptor, R, I, C ) = true),T),
	\+ holdsAt((role_of(A, acceptor, R, I, C ) = true),T-1). %, write('acc'), nl.
	
holdsAt(obl(L, syncReq(L, A, V, R, I, C)) = true, T) :-
	holdsAt(per(L, syncReq(L, A, V, R, I, C)) = true, T),
	chosen(V, R, I, C, T-1),
	holdsAt((role_of(A, acceptor, R, I, C ) = true),T),
	\+ holdsAt((role_of(A, acceptor, R, I, C ) = true),T-1).
	
% the new acceptor can ack the sync with either a yes (by voting FOR the currently chosen value)
% or by voting for anything else (eg, 'no'). This initiates a voted&reportedVote or an obligation
% for the leader to revise, respectively. Either way it terminates the sync.
initiates(syncAck(A, V, R, I, C), (voted( A, (R, B), V, I, C ) = true), T) :-
	holdsAt(pow(A, syncAck(A, V, R, I, C)) = true, T),
	%holdsAt(per(A, syncAck(A, V, R, I, C)) = true, T),
	% setOfHighestVotes(I, C, T, AllVotes), 
	% max_in(AllVotes, Max),
	% getxr(Max, R), 
	% getxb(Max, B),
	% getxv(Max, V). %,
	%write('Here1'), nl,
	% have to use this not chosen() because the set of acceptors has changed
	highestVote(V, R, B, I, C, T).
	%write('Initiated voted'), nl.
	
initiates(syncAck(A, V, R, I, C), (reportedVote( A, (R, B), V, (R,B), I, C ) = true), T) :-
	holdsAt(pow(A, syncAck(A, V, R, I, C)) = true, T),
	%holdsAt(per(A, syncAck(A, V, R, I, C)) = true, T),
	% setOfHighestVotes(I, C, T, AllVotes), 
	% max_in(AllVotes, Max),
	% getxr(Max, R), 
	% getxb(Max, B),
	% getxv(Max, V). %,
	%write('Here2'), nl,
	% have to use this not chosen() because the set of acceptors has changed
	highestVote(V, R, B, I, C, T).
	%write('Initiated reportedVote'), nl.
	
% initiates(syncAck(A, 'no', R, I, C), (voted( A, (R, 0), null, I, C ) = true), T) :-
	% holdsAt(pow(A, syncAck(A, 'no', R, I, C)) = true, T),
	% highestVote(V, R, B, I, C, T), write('initiated a null vote'), nl.	

initiates(syncAck(A, 'no', R, I, C), (reportedVote( A, (R, 0), null, (R,B), I, C ) = true), T) :-
	holdsAt(pow(A, syncAck(A, 'no', R, I, C)) = true, T),
	highestVote(V, R, B, I, C, T). %, write('initiated a null reportedVote for '), write(A), nl.
	
% note that this means 'no' is not a valid value...
% initiates(syncAck(A, 'no', R, I, C), obl(L, revise(L, I, C)) = true , T) :-
	% %numberOfVotesForValue(V, R, I, C, T, NumberFor, NumberAgainst), write('For:'), write(NumberFor), nl, write('Against:'), write(NumberAgainst), nl,
	% setof((Agent,RevBall,Value), Agent^RevBAll^Value^B^I^C^holdsAt( (reportedVote(Agent,RevBall,Value,B,I,C ) = true), T ), AllVotes1),
	% write('AllVotes:'), write(AllVotes1), nl,
	% holdsAt(per(A, syncAck(A, 'no', R, I, C)) = true, T), 
	% holdsAt(per(L, revise( L, I, C)) = true, T), 
	% holdsAt(sync(A, _, R, I, C) = true, T).
	% %NumberFor is (NumberAgainst - 1).	

% obl to revise if the reply is no and there was a possibility that you had to revise
% note that multiple syncAck's can't be in the same timestep if this is to work
initiates(syncAck(A, 'no', R, I, C), obl(L, revise(L, I, C)) = true , T) :-
	holdsAt(per(A, syncAck(A, 'no', R, I, C)) = true, T),
	holdsAt(per(L, revise( L, I, C)) = true, T),
	holdsAt(possibleAddRevision(R, I, C) = true, T).
	
	
% there's a possibility of revision if anyone is being synched (so a val *was* chosen)
% and if the number of votes for and not-for that val are equal before the syncAck
holdsAt(possibleAddRevision(R, I, C) = true, T) :-
	holdsAt(sync(_, Val, R, I, C) = true, T),
	numberOfVotesForValue(V, R, I, C, T, NumberFor, NumberAgainst),
	NumberFor = NumberAgainst.
	
% there's a possibility of revision if a value has been chosen
% and if the number of votes for and not-for that val are equal before the acceptor leaves
holdsAt(possibleRemRevision(V, R, I, C) = true, T) :- %write('Checking for possibleRemRevision'), nl,
	% have to use this not chosen() because the set of acceptors has changed
	highestVote(V, R, B, I, C, T), %write('Got value !'), nl,
	numberOfVotesForValue(V, R, I, C, T, NumberFor, NumberAgainst), %write('Got #Votes'), nl,
	NumberFor = NumberAgainst. %, write('Possible Rem Revision'), nl.
	
% leader should revise if new member being sync'd doesn't agree (MAYBE)
%initiates(syncAck(A, 'no', R, I, C), obl(L, revise(L, I, C)) = true , T) :-
	%%holdsAt(pow(A, syncAck(A, _, R, I, C)) = true, T), %write('powSyn'), nl,
	%holdsAt(per(A, syncAck(A, 'no', R, I, C)) = true, T), %write('perSyn'), nl,
	%%holdsAt(pow(L, revise( L, I, C)) = true, T), %write('powRev'), nl,
	%holdsAt(per(L, revise( L, I, C)) = true, T). %,% write('perRev'), nl,
	% setOfHighestVotes(I, C, T, AllVotes), 
	% max_in(AllVotes, Max), %write(Max), nl,
	% getxr(Max, R),  %write(R), nl,
	% getxb(Max, B), %write(B), nl,
	% getxv(Max, V), %write(V), nl,
	%write('Here3'), nl,
	% highestVote(V, R, I, C, T),
	% not(Ack = V). %, write(AV), nl.
	
holdsAt(pow(A, syncAck(A, Ack, R, I, C)) = true, T) :-
	holdsAt((role_of(A, acceptor, R, I, C ) = true),T), %.
	holdsAt(sync(A, V, R, I, C) = true, T),
	(Ack = V ; Ack = 'no').
holdsAt(per(A, syncAck(A, Ack, R, I, C)) = true, T) :- %write('perSyn? '),
	holdsAt(pow(A, syncAck(A, Ack, R, I, C)) = true, T). %, write('perSyn for '), write(A), nl.
	%holdsAt(sync(A, _, R, I, C) = true, T). %, write('yes').
	
terminates(syncAck(A, Ack, R, I, C), sync(A, _, R, I, C) = true, T) :-
	holdsAt(pow(A, syncAck(A, Ack, R, I, C)) = true, T). %,
	%holdsAt(per(A, syncAck(A, V, R, I, C)) = true, T).
	
% A has an obligation to syncAck if a sync exists
holdsAt(obl(A, syncAck(A, Ack, R, I, C)) = true,T):-
	holdsAt(per(A, syncAck(A, Ack, R, I, C)) = true, T),
	holdsAt(sync(A, V, R, I, C) = true, T),
	(Ack = V ; Ack = 'no'). %,
	%highestVote(V, R, B, I, C, T). %,
	% setOfHighestVotes(I, C, T, AllVotes), 
	% max_in(AllVotes, Max),
	% getxr(Max, R), 
	% getxb(Max, B),
	% getxv(Max, V).
	
%% this shows obligation to revise if you don't have them displayed in info
% holdsAt(pow(L, foo(L)) = true, T) :-
	% holdsAt(obl(L, revise(L, I, C)) = true , T).
% holdsAt(per(L, foo(L)) = true, T):-
	% holdsAt((role_of(L, leader, _, _, _ ) = true),T).

% happens(X) initiates lazy sanction against A if pow & no per
% FIXME work out how to do this properly if we keep on with prolog
initiates(X, sanction(A)=true,T) :-
	holdsAt(pow(A, X)=true,T),
	%write(A), write(' has pow to '), write(X), nl,
	\+ holdsAt(per(A, X)=true,T). %, write('but no per'),nl.

% this might generate A LOT of fluents, because these are never going to be terminated...	
holdsAt((quorum_size( R, I, C) = Size),T) :-
	setof((Acceptor), (holdsAt(role_of(Acceptor, acceptor, R, I, C)=true, T)), Acceptors), %write('Acc:'), write(Acceptors),nl,
	length(Acceptors, Count), %write('Count:'), write(Count),
	%% round up ...
	%Size is (Count+1) / 2. %, write('QSize:'), write(Size).
	%% or integer divide.
	Size is (Count//2) +1. %, write('QSize:'), write(Size).
	
%% sorting out holdsAt relations
holdsAt( pow(Agent, Action) = false, T ) :-
	\+ holdsAt( pow(Agent, Action) = true, T ).
	
holdsAt( per(Agent, Action) = false, T ) :-
	\+ holdsAt( per(Agent, Action) = true, T ).
	
%% obligation stuff
holdsAt( obl(Agent, Action) = true, T ) :-
	holdsAt( oblig( Role, R, I, C, Action ) = true, T),
	holdsAt( role_of( Agent, Role, R, I, C ) = true, T).
	
% youre the leader in all issues if youre the leader in any
holdsAt((role_of(Agent, leader, Issue, Cluster)=true),T) :-
	holdsAt((role_of(Agent, leader, _, Issue, Cluster)=true),T).
	
holdsAt((role_of(Agent, proposer, Issue, Cluster)=true),T) :-
	holdsAt((role_of(Agent, proposer, _, Issue, Cluster)=true),T).

	
holdsAt((able(Agent, Action)=true), T) :-
	holdsAt( pow(Agent, Action) = true, T ). %,
	%holdsAt( per(Agent, Action) = true, T ).

	
	
/************************ 
 * AUXILIARY PREDICATES *
 ************************/
% ----- retrieve information about each agent in a coherent manner

holdsAt(description_of(Agent, State), T) :-
	findall((Role,Revision,Issue,Cluster),      holdsAt(   role_of(Agent,Role,Revision,Issue,Cluster) = true, T ), Roles),
	%findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities),
	findall(PAction,   holdsAt( pow(Agent, PAction) = true,   T ), Powers),
	findall(PerAction, holdsAt( per(Agent, PerAction) = true, T ), Permissions),
	%% These two lines strip out the duplicates, but give unbound variables instead of the string when written...
	%findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities1),
	%removeDup(Abilities1, Abilties),
	findall(((RevBall,Value),(Issue,Cluster)),	holdsAt(	voted( Agent, RevBall, Value, Issue, Cluster ) = true, T), Votes),
	findall(OAction,   holdsAt( obl(Agent, OAction) = true,   T ), Obligations),
	holdsAt( sanction(Agent) = Sanctions, T ),
	append([pow(Powers)], [per(Permissions)], PowPer),
	append([Agent,roles(Roles)], PowPer, Temp1),
	append(Temp1, [obl(Obligations)], Temp2),
	append(Temp2, [sanctions(Sanctions)], Temp3),
	append( Temp3,[votes(Votes)], State ).
	% findall((Role,Revision,Issue),      holdsAt(   role_of(Agent,Role,Revision,Issue,Cluster) = true, T ), Roles),
	% findall(PAction,   holdsAt( pow(Agent, PAction) = true,   T ), PActions),
	% findall(PerAction, holdsAt( per(Agent, PerAction) = true, T ), PerActions),
	% findall((RevBall,Value),	holdsAt(	voted( Agent, RevBall, Value, Issue, Cluster ) = true, T), Votes),
	% append([Agent,roles(Roles)],     [permissions(PerActions)], Temp1),
	% append(Temp1, [powers(PActions)],   Temp2),
	% append(Temp2, [votes(Votes)], State ).

holdsAt(description_of(Agent, State), T) :-
	findall((Role,Revision,Issue),      holdsAt(   role_of(Agent,Role,Revision,Issue,Cluster) = true, T ), Roles),
	findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities),
	%% These two lines strip out the duplicates, but give unbound variables instead of the string when written...
	%findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities1),
	%removeDup(Abilities1, Abilties),
	findall((RevBall,Value),	holdsAt(	voted( Agent, RevBall, Value, Issue, Cluster ) = true, T), Votes),
	append([Agent,roles(Roles)],     [abilities(Abilities)], Temp1),
	append( Temp1,[votes(Votes)], State ).
	
holdsAt(long_description_of(Agent, State), T) :-
	findall((Role,Revision,Issue,Cluster),      holdsAt(   role_of(Agent,Role,Revision,Issue,Cluster) = true, T ), Roles),
	findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities),
	%% These two lines strip out the duplicates, but give unbound variables instead of the string when written...
	%findall(Action,		holdsAt(	able(Agent,Action) = true, T), Abilities1),
	%removeDup(Abilities1, Abilties),
	findall(((RevBall,Value),(Issue,Cluster)),	holdsAt(	voted( Agent, RevBall, Value, Issue, Cluster ) = true, T), Votes),
	append([Agent,roles(Roles)],     [abilities(Abilities)], Temp1),
	append( Temp1,[votes(Votes)], State ).
	% findall((Role,Revision,Issue),      holdsAt(   role_of(Agent,Role,Revision,Issue,Cluster) = true, T ), Roles),
	% findall(PAction,   holdsAt( pow(Agent, PAction) = true,   T ), PActions),
	% findall(PerAction, holdsAt( per(Agent, PerAction) = true, T ), PerActions),
	% findall((RevBall,Value),	holdsAt(	voted( Agent, RevBall, Value, Issue, Cluster ) = true, T), Votes),
	% append([Agent,roles(Roles)],     [permissions(PerActions)], Temp1),
	% append(Temp1, [powers(PActions)],   Temp2),
	% append(Temp2, [votes(Votes)], State ).
	
holdsAt(description_of_issue(Issue, State), T) :-
	findall(PreVoteBallot,	holdsAt( (pre_vote(PreVoteBallot, Issue, Cluster) = true), T), PreVotes),
	append([], Issue, Temp1),
	append(Temp1, preVotes(PreVotes), State).
/*************
 * NARRATIVE *
 *************/
% ----- this is an example narrative, that is, a list of externally observable
% ----- events of a run of a Voting Protocol
% ----- cAgent opens the session 
/*
happens( open_session(cAgent, sesh),     1).
happens( propose(pAgent, m1),            2).
happens( second(sAgent, m1),             3).
happens( open_ballot(cAgent, m1),        4).
happens( vote(cAgent, m1, aye),          5).
happens( vote(pAgent, m1, aye),          6).
happens( vote(sAgent, m1, nay),          7).
happens( vote(vAgent1, m1, nay),         8).
happens( vote(vAgent2, m1, nay),         9).
happens( close_ballot(cAgent, m1),      10).
happens( declare(cAgent, m1, carried),  11).
happens( close_session(cAgent, sesh),  12).
*/
/*
happens( open_session(cAgent, sesh),     1).
happens( propose(pAgent, m1),            2).
happens( second(sAgent, m1),             3).
happens( open_ballot(cAgent, m1),        4).
happens( vote(pAgent, m1, aye),          5).
happens( vote(sAgent, m1, nay),          6).
happens( vote(vAgent1, m1, nay),         7).
happens( revoke(sAgent,m1),              8).
happens( vote(sAgent, m1, aye),          9).
happens( close_ballot(cAgent, m1),      10).
happens( declare(cAgent, m1, not_carried),  11).
happens( close_session(cAgent, sesh),   12).
*/
/*
happens( open_session(cAgent, sesh),     1).
happens( propose(pAgent, m1),            2).
happens( second(sAgent, m1),             3).
happens( open_ballot(cAgent, m1),        4).
happens( vote(pAgent, m1, aye),          5).
happens( vote(sAgent, m1, nay),          6).
happens( vote(vAgent1, m1, nay),         7).
happens( close_ballot(cAgent, m1),       8).
happens( declare(cAgent, m1, carried),   9).
happens( close_session(cAgent, sesh),   10).
*/
/*
happens( open_session(cAgent, sesh),     1).
happens( propose(pAgent, m1),            2).
happens( second(sAgent, m1),             3).
happens( open_ballot(cAgent, m1),        4).
happens( vote(cAgent, m1, nay),          5).
happens( vote(cAgent, m1, aye),          6).
happens( close_ballot(cAgent, m1),       7).
happens( declare(cAgent, m1, carried),   8).
happens( propose(pAgent, m2),            9).
happens( close_session(cAgent, sesh),   10).
happens( start, 0 ).
happens( end, 13 ).
*/
/******************
 * SAMPLE QUERIES *
 *****************/
dump_fromto( Agents, StartClock, EndClock ) :-
	StartClock > EndClock,
	dump_list( Agents, StartClock ), nl.
dump_fromto( Agents, StartClock, EndClock ) :-
	write( 'Clock Time ' ), write( StartClock ), nl,
	dump_list( Agents, StartClock ),
	nl, nl, happens( Event, StartClock ), write( Event ), nl,
	NextClock is StartClock + 1, nl,
	dump_fromto( Agents, NextClock, EndClock ).
dump_list( [], _ ).
dump_list( [Agent|T], Clock ) :-
	dump( Agent, Clock, State ),
	write( Agent ), nl,
	write( State ), nl,nl,
	dump_list( T, Clock ).
dump( Agent, Clock, State ) :-
	\+ verbose,
	holdsAt( description_of( Agent, State ), Clock ).
dump( Agent, Clock, State ) :-
	verbose,
	holdsAt( long_description_of( Agent, State ), Clock ).
dumpIssue( Issue, Clock, IssueState ) :-
	holdsAt(description_of_issue(Issue, IssueState), Clock),
	write('Issue:'),
	write(Issue), nl, write(IssueState).
% d :-
	% dump_fromto( [ag1,ag2,ag3,ag4,ag5,ag6], 1, 12 ).
% d(N) :-
	% dump_fromto( [ag1,ag2,ag3,ag4,ag5,ag6], 1, N ).
d :-
	setOfAllAgents(Agents),
	dump_fromto( Agents, 1, 12 ).
d(N) :-
	setOfAllAgents(Agents),
	dump_fromto( Agents, 1, N ).


/*
	dump_fromto( [cAgent,pAgent,sAgent,vAgent1,vAgent2], 1, 13 ).
*/
/*
dump_fromto( [cAgent,pAgent,sAgent,vAgent1,vAgent2], 0, 12 ).
holdsAt( description_of( cAgent, State ), 0 ).
holdsAt( description_of( cAgent, State ), 1 ).
holdsAt( description_of( pAgent, State ), 0 ).
holdsAt( description_of( pAgent, State ), 1 ).
holdsAt( description_of( sAgent, State ), 0 ).
holdsAt( description_of( sAgent, State ), 1 ).
holdsAt( description_of( vAgent, State ), 0 ).
holdsAt( description_of( vAgent, State ), 1 ).
*/
