%% Narrative to show removal of multiple agents, and how consensus can be rebuilt (fragmentation)
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
initially( role_of(ag5,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag6,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag7,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag8,acceptor, 0, issue, cluster) = true ).

%%set previous votes - cluster1 has decided on a
initially( voted(ag1, (0,1),a,		issue, cluster) = true ).
initially( voted(ag2, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag5, (0,1),a,		issue, cluster) = true ).
initially( voted(ag6, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag7, (0,1),a,		issue, cluster) = true ).
initially( voted(ag8, (0,1),a,		issue, cluster) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	issue, cluster) = true ).
initially( reportedVote(ag5, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag6, (0,1),a,(0,1), 	issue, cluster) = true ).
initially( reportedVote(ag7, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag8, (0,1),a,(0,1),		issue, cluster) = true ).

%% first we'll deal with the new cluster
%happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens(leaveCluster([ag5,ag6,ag7,ag8], 						cluster ),	1).
happens(arrogateLeadership( ag5, 						issue,	cluster2 ),	2).
% we presume that if anyone objects, they can sort it out
happens( addRole(ag5, [ag5, ag6, ag7, ag8], acceptor, 0, issue,	cluster2 ),	3).
happens( addRole(ag5, ag5, proposer, 			any,	issue,	cluster2 ),	4).
happens( request0a( ag5, b, 							issue,	cluster2 ),	5).
happens( prepare1a( ag5, (0,1), 						issue,	cluster2 ),	6).
happens( response1b(ag5, ( ag5, (0,0),null ), 	(0,1),	issue,	cluster2 ), 7).
happens( response1b(ag6, ( ag6, (0,0),null ), 	(0,1),	issue,	cluster2 ), 7).
happens( response1b(ag7, ( ag7, (0,0),null ),	(0,1),	issue,	cluster2 ), 7).
happens( response1b(ag8, ( ag8, (0,0),null ), 	(0,1),	issue,	cluster2 ), 7).
happens( submit2a(	ag5, (0,1), b,						issue,	cluster2 ),	8).
happens( vote2b( 	ag5, (0,1), b,						issue,	cluster2 ),	9).
happens( vote2b( 	ag6, (0,1), b,						issue,	cluster2 ),	9).
happens( vote2b( 	ag7, (0,1), b,						issue,	cluster2 ),	9).
happens( vote2b( 	ag8, (0,1), b,						issue,	cluster2 ),	9).
% cluster2 has now chosen a value for issue
%% now we deal with cluster
happens( revise(	ag1,								issue,	cluster ),	10).
happens( request0a( ag1, c, 							issue,	cluster ),	11).
happens( prepare1a( ag1, (1,1), 						issue,	cluster ),	12).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	issue,	cluster ), 	13).
happens( response1b(ag2, ( ag2, (1,0),null ), 	(1,1),	issue,	cluster ), 	13).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,1),	issue,	cluster ), 	13).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,1),	issue,	cluster ), 	13).
happens( submit2a(	ag1, (1,1), c,						issue,	cluster ),	14).
happens( vote2b( 	ag1, (1,1), c,						issue,	cluster ),	15).
happens( vote2b( 	ag2, (1,1), c,						issue,	cluster ),	15).
happens( vote2b( 	ag3, (1,1), c,						issue,	cluster ),	15).
happens( vote2b( 	ag4, (1,1), c,						issue,	cluster ),	15).
% cluster has revised and chosen a new value
