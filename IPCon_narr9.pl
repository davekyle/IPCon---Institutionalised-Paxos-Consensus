%% Narrative to show loss of multiple agents (in breach of classicPaxos assumptions) and the consequence of this
verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4, ag5]).
 
%%Make a leader
initially( role_of(ag1, leader, any, issue, cluster) = true ).
initially( role_of(ag1, proposer, issue, cluster) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag2,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag3,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag4,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag5,acceptor, 0, issue, cluster) = true ).
%%set previous votes
initially( voted(ag1, (0,1),a,		issue, cluster) = true ).
initially( voted(ag2, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag3, (0,1),a,		issue, cluster) = true ).
% ag4 & 5 didn't vote

happens( request0a( ag1, a, 							issue,	cluster ),	1).
happens( prepare1a( ag1, (0,2), 						issue,	cluster ),	2).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	issue,	cluster ), 	3).
happens( response1b(ag2, ( ag2, (0,1),a ), 		(0,2),	issue,	cluster ), 	4).
happens( response1b(ag3, ( ag3, (0,1),a ), 		(0,2),	issue,	cluster ), 	5).
happens( response1b(ag4, ( ag4, (0,0),null ),	(0,2),	issue,	cluster ), 	6).
happens( response1b(ag5, ( ag5, (0,0),null ),	(0,2),	issue,	cluster ), 	7).
happens( leaveCluster(ag3,										cluster ),	8).
happens( request0a( ag1, b, 							issue,	cluster ),	9).
happens( prepare1a( ag1, (0,3), 						issue,	cluster ),	10).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,3),	issue,	cluster ), 	11).
happens( response1b(ag2, ( ag2, (0,1),a ), 		(0,3),	issue,	cluster ), 	12).
happens( response1b(ag4, ( ag4, (0,0),null ),	(0,3),	issue,	cluster ), 	13).
happens( response1b(ag5, ( ag5, (0,0),null ),	(0,3),	issue,	cluster ), 	14).
% possibly counter-intuitively, the loss of ag2 has no effect on A being the only safe value
% (though a may not be chosen again if 4,5 don't vote for it...)
happens( submit2a(	ag1, (0,3), b,						issue,	cluster ),	15).
happens( vote2b( 	ag1, (0,3), b,						issue,	cluster ),	16).
% but, if ag2 is replaced by ag6 who has not previously voted...
% the quorumsize has remained unchanged at all times, but the set of previously voted changes
%%
% if now ag2 leaves as well, a is no longer chosen. This is in contrast to previously shown
% situation where choice-agent left, quorum size changed, but choice remained (narr7)
happens( leaveCluster(ag2,										cluster ),	17).
happens( prepare1a( ag1, (0,4), 						issue,	cluster ),	18).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,4),	issue,	cluster ), 	19).
happens( response1b(ag4, ( ag4, (0,0),null ),	(0,4),	issue,	cluster ), 	20).
happens( response1b(ag5, ( ag5, (0,0),null ),	(0,4),	issue,	cluster ), 	21).
%% now that 2 agents have left the quorum, we have broken assumptions of classic Paxos
%% ie, we had 2N+1 agents, and N have failed.
happens( submit2a(	ag1, (0,4), b,						issue,	cluster ),	22).
happens( vote2b( 	ag1, (0,4), b,						issue,	cluster ),	23).
