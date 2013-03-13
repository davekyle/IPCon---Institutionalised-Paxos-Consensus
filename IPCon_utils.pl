:- use_module(library(lists)).

numberOfVotesForValue(Val, R, I, C, T, NumberFor, NumberAgainst) :-
	setOfHighestVotes(I, C, T, AllVotes), write('All:'), write(AllVotes), nl,
	findall((Agent,(R,_),Val), member((Agent,(R,_),Val), AllVotes), VotesForVal), write('For:'), write(VotesForVal), nl,
	findall((Agent2,(R,_),Q), (member((Agent2,(R,_),Q), AllVotes), not(Q=Val)), VotesAgainstVal), write('Against:'), write(VotesAgainstVal), nl,
	length(VotesForVal, NumberFor),
	length(VotesAgainstVal, NumberAgainst).

highestVote(V, R, B, I, C, T) :-
	setOfHighestVotes(I, C, T, AllVotes), 
	max_in(AllVotes, Max),
	getxr(Max, R), 
	getxb(Max, B),
	getxv(Max, V).

% check to see if V has been chosen in R I C at T
%% note: doesn't actually return true if a value was chosen but then a subsequent ballot
%% fails to confirm this (eg due to learner/acceptor failure in reporting votes)
chosen(V, R, I, C, T) :-
	setOfHighestVotes(I, C, T, AllVotes),
	%write('Votes:'), write(AllVotes), nl,
	max_in(AllVotes, Max),
	%write('Max:'), write(Max), nl,
	getxr(Max, MaxR), 
	getxb(Max, MaxB),
	getxv(Max, MaxV),
	MaxV = V,
	MaxR = R,
	findall((MaxV, MaxR, MaxB), member((_,(MaxR, MaxB), MaxV),AllVotes), HighestVotes),
	%write('Highest:'), write(HighestVotes), nl,
	length(HighestVotes, Length),
	holdsAt((quorum_size( MaxR, I, C) = Size),T),% write('@'), write(T), write(' L:'), write(Length), write(' QS:'), write(Size), nl,
	Length >= Size, !.


% find set of highest votes
setOfHighestVotes(I, C, T, AllVotes) :-
	setof((Agent,RevBall,Value), B^holdsAt( (reportedVote(Agent,RevBall,Value,B,I,C ) = true), T ), AllVotes1), %write('gotVotes'), nl,
	%write('AllVotes:'), write(AllVotes1), nl,
	%setof((Agent,RevBall,Value), I^C^holdsAt( voted( Agent, RevBall, Value, I, C ) = true, T ), AllVotes1),
	%% Get set of agents that have had a reply about them %%
	findall( (Agent) , member((Agent,RevBall,Value),AllVotes1), ReportedAgents), %write('gotAgents'), nl,
	%write('Reported Agents:'), write(ReportedAgents), write(' // '),
	%% Don't care how many we get in this case.
	%%length(ReportedAgents, NumberOfAgents), NumberOfAgents >= Size,  %write('gotNumber'), nl,
	%write(ReportedAgents), nl,
	clean(ReportedAgents, AllVotes1, AllVotes2),
	removeDup(AllVotes2, AllVotes).
	
%% unused
%% Find highest vote per agent ONLY, at a time T
cleanT(AllVotes, T, Result) :-
	setof((Agent), RB^V^holdsAt(voted( Agent, RB, V, I, C ) = true, T) , Agents),
	%write('Agents:'), write(Agents), nl,
	clean(Agents, AllVotes, Result).


%%clean(Agents,AllVotes,HighestVotesByEachAgent)
clean([], AllVotes, []).
clean([H|T], AllVotes, L) :-
	%write('AllVotes:'), write(AllVotes), nl,
	%write('Checking '), write(H), write('...'),
	max_inA(H, AllVotes, Max),
	%write('A:'), write(H), write(' // Max:'), write(Max), nl,
	append([Max],L1, L),
	clean(T,AllVotes, L1).
	
%remove duplicates from a list
removeDup([],[]).
removeDup([H|T],[H|Out]) :-
    not(member(H,T)),
    removeDup(T,Out).
removeDup([H|T],Out) :-
    member(H,T),
    removeDup(T,Out).
	
	
% reverse a list
reverse([X|Y],Z,W) :- reverse(Y,[X|Z],W).
reverse([],X,X).	

% find all subsets of length N
subsetN( _, S, N, S ) :-
	%write('In1'), nl,
    length( S, N ), %write('lengthMatched'), nl,
	!.

subsetN( [H|T], Sofar, N, S ) :-
	%write('In2'),nl,
	% write('H:'), write(H),
	% write(' // T:'), write(T),
	% write(' // SoFar:'), write(SoFar),
	% write(' // N:'), write(N),
	%write('firstS:'), write(S), nl,
    M is N - 1,
    subsetN( T, [], M, S1 ), %write('S1:'), write(S1), nl,
    append( [H|Sofar], S1, S ). %write('lastS:'), write(S),nl

subsetN( [_|T], Sofar, N, S ) :-
	%write('In3'),nl,
    subsetN( T, Sofar, N, S ). % write('Done3'), nl.
	
	
%% Find max
max_in([H|T], Result) :-
	getMax(T,H,Result).

getMax([],SoFar,SoFar).

getMax([H|T], SoFar, R) :-
	H = (_,(A,_),_),
	SoFar = (_,(B,_),_),
	A>B, !,
	getMax(T, H, R).
	
getMax([H|T], SoFar, R) :-
	H = (_,(HR,HB),_),
	SoFar = (_,(SR,SB),_),
	HR=SR, HB>SB, !,
	getMax(T, H, R).

getMax([_|T],SoFar,R) :-
	getMax(T,SoFar,R).
	
	
max_inA(Ag, [H|T], Result) :-
	H = (Ag,(A,_),_),
	getMaxA(Ag, T,H,Result).

max_inA(Ag, [H|T], Result) :-
	H = (Bg,(A,_),_),
	not(Ag == Bg),
	getMaxA(Ag, T,[],Result).

getMaxA(Ag, [H|T],[],Result):-
	getMaxA(Ag, T, H, Result).
	
getMaxA(Ag, [],SoFar,SoFar).

getMaxA(Ag, [H|T], SoFar, R) :-
	H = (Ag,(A,_),_),
	SoFar = (Ag,(B,_),_),
	A>B, !,
	getMaxA(Ag, T, H, R).
	
getMaxA(Ag, [H|T], SoFar, R) :-
	H = (Ag,(HR,HB),_),
	SoFar = (Ag,(SR,SB),_),
	HR=SR, HB>=SB, !,
	getMaxA(Ag, T, H, R).

getMaxA(Ag,[H|T],SoFar,R) :-
	H = (Bg,(_,_),_),
	not(Ag == Bg),
	getMaxA(Ag,T,SoFar,R).
	
getxr( (_, (XR,_), _ ), XR ).
getxb( (_,(_,XB),_),XB).
getxv( (_,_,XV),XV).

pickNext( XR, R ) :-
	var( R ),
	gen1by1( XR, R ).
	
gen1by1( R, R ).
gen1by1( R, RR ) :-
	R1 is R + 1,
	gen1by1( R1, RR ).
%%FAIL below%%
% max_in( [H|T], Z) :-
	% max_in(H) = X,
	% max_in(T) = Y,
	% maxRB(X,Y,Z).
% max_in( [H], H). % single term
% max_in( [], noRevision). %empty list
% maxRB(X,Y,X) :-
	% X = (_,(RX,BX),_),
	% Y = (_,(RY,BY),_),
	% RX >= RY.
	
% maxRB(X,Y,X) :-
	% X = (_,(RX,BX),_),
	% Y = (_,(RX,BY),_),
	% BX >= BY.
	
% maxRB(X,Y,Y) :-
	% X = (_,(RX,BX),_),
	% Y = (_,(RY,BY),_),
	% RY > RX.
	
% maxRB(X,Y,Y) :-
	% X = (_,(RX,BX),_),
	% Y = (_,(RX,BY),_),
	% BY > BX.