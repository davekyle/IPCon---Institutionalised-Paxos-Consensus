%% Narrative to show addition of multiple agents, and how consensus can be rebuilt (aggregation)
verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4, ag5, ag6, ag7, ag8]).
 
%%Make a leader
initially( role_of(ag1, leader, any, issue, cluster) = true ).
initially( role_of(ag1, proposer, issue, cluster) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag2,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag3,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag4,acceptor, 0, issue, cluster) = true ).

initially( role_of(ag5,leader, any, issue, cluster2) = true ).
initially( role_of(ag5, proposer, issue, cluster2) = true ).
initially( role_of(ag5,acceptor, 0, issue, cluster2) = true ).
initially( role_of(ag6,acceptor, 0, issue, cluster2) = true ).
initially( role_of(ag7,acceptor, 0, issue, cluster2) = true ).
initially( role_of(ag8,acceptor, 0, issue, cluster2) = true ).
%%set previous votes - cluster1 has decided on a
initially( voted(ag1, (0,1),a,		issue, cluster) = true ).
initially( voted(ag2, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag3, (0,1),a,		issue, cluster) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	issue, cluster) = true ).
initially( reportedVote(ag3, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag4, (0,0),null,(0,1),	issue, cluster) = true ).
% ag4 hasn't voted
%%set previous votes - cluster2 has decided on b
initially( voted(ag5, (0,4),b,		issue, cluster2) = true ).
initially( voted(ag6, (0,4),b, 		issue, cluster2) = true ).
initially( voted(ag7, (0,4),b,		issue, cluster2) = true ).
initially( voted(ag8, (0,4),b,		issue, cluster2) = true ).
initially( reportedVote(ag5, (0,4),b,(0,4),		issue, cluster2) = true ).
initially( reportedVote(ag6, (0,4),b,(0,4), 		issue, cluster2) = true ).
initially( reportedVote(ag7, (0,4),b,(0,4),		issue, cluster2) = true ).
initially( reportedVote(ag8, (0,4),b,(0,4),		issue, cluster2) = true ).

%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	issue,	cluster ), 	3).
%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,4),	issue,	cluster ), 	19).
happens( leaveCluster([ag5, ag6, ag7, ag8],						cluster2),	1).
happens( addRole(ag1, [ag5, ag6, ag7, ag8], acceptor, 0, issue,	cluster ),	2).
happens( addRole(ag1, ag5, proposer, 			any,	issue,	cluster ),	2).
happens( syncReq(	ag1, ag5, 	a, 					0,	issue,	cluster ),	3).
happens( syncReq(	ag1, ag6, 	a, 					0,	issue,	cluster ),	3).
happens( syncReq(	ag1, ag7, 	a, 					0,	issue,	cluster ),	3).
happens( syncReq(	ag1, ag8, 	a, 					0,	issue,	cluster ),	3).
happens( syncAck(	ag5,		no,					0,	issue,	cluster ),	4).
happens( syncAck(	ag6,		no,					0,	issue,	cluster ),	5).
happens( syncAck(	ag7,		no,					0,	issue,	cluster ),	6).
happens( syncAck(	ag8,		no,					0,	issue,	cluster ),	7).
happens( nothing														,	8).
happens( nothing														,	9).
happens( revise(	ag1,								issue,	cluster ),	10).