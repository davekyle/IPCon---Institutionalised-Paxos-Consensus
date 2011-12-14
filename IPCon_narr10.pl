%% Narrative to show addition of multiple agents, and how consensus can be rebuilt (aggregation)
verbose :-
 true.
 
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
happens( syncAck(	ag5,		a,					0,	issue,	cluster ),	4).
happens( syncAck(	ag6,		no,					0,	issue,	cluster ),	5).
% dont need to do these ones, revision will remove them
%happens( syncAck(	ag7,		no,					0,	issue,	cluster ),	6).
%happens( syncAck(	ag8,		no,					0,	issue,	cluster ),	7).
happens( revise(	ag1,								issue,	cluster ),	6).
happens( request0a( ag1, a, 							issue,	cluster ),	7).
happens( request0a( ag5, b, 							issue,	cluster ),	7).
happens( request0a( ag5, c, 							issue,	cluster ),	7).
happens( prepare1a( ag1, (1,1), 						issue,	cluster ),	8).
% we only care about this revision, so no *need* to reply about others.
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag2, ( ag2, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag5, ( ag5, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag6, ( ag6, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag7, ( ag7, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( response1b(ag8, ( ag8, (1,0),null ), 	(1,1),	issue,	cluster ), 	9).
happens( submit2a(	ag1, (1,1), a,						issue,	cluster ),	10).
happens( vote2b( 	ag2, (1,1), a,						issue,	cluster ),	11).
%happens( vote2b( 	ag3, (1,1), a,						issue,	cluster ),	11).
happens( prepare1a( ag1, (1,2), 						issue,	cluster ),	12).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag2, ( ag2, (1,1),a ),	 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag3, ( ag3, (0,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag5, ( ag5, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag6, ( ag6, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag7, ( ag7, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( response1b(ag8, ( ag8, (1,0),null ), 	(1,2),	issue,	cluster ), 	13).
happens( submit2a(	ag1, (1,2), b,						issue,	cluster ),	14).
happens( vote2b( 	ag6, (1,2), b,						issue,	cluster ),	15).
happens( vote2b( 	ag7, (1,2), b,						issue,	cluster ),	15).
happens( prepare1a( ag1, (1,3), 						issue,	cluster ),	16).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag2, ( ag2, (1,1),a ),	 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag3, ( ag3, (0,0),null ), 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag5, ( ag5, (1,0),null ), 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag6, ( ag6, (1,2),b ), 		(1,3),	issue,	cluster ), 	17).
happens( response1b(ag7, ( ag7, (1,2),b ),	 	(1,3),	issue,	cluster ), 	17).
happens( response1b(ag8, ( ag8, (1,0),null ), 	(1,3),	issue,	cluster ), 	17).
happens( submit2a(	ag1, (1,3), c,						issue,	cluster ),	18).
happens( vote2b( 	ag1, (1,3), c,						issue,	cluster ),	19).
happens( vote2b( 	ag3, (1,3), c,						issue,	cluster ),	19).
happens( vote2b( 	ag4, (1,3), c,						issue,	cluster ),	19).
happens( vote2b( 	ag5, (1,3), c,						issue,	cluster ),	19).
happens( vote2b( 	ag8, (1,3), c,						issue,	cluster ),	19).
%% chosen

















