%% Narrative to show loss of agent if quorum changes and agent wasnt in choicequorum
verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4]).
 
%%Make a leader
initially( role_of(ag1, leader, any, issue, cluster) = true ).
initially( role_of(ag1, proposer, issue, cluster) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag2,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag3,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag4,acceptor, 0, issue, cluster) = true ).
%%set previous votes
initially( voted(ag1, (0,1),a,		issue, cluster) = true ).
initially( voted(ag2, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag3, (0,1),a,		issue, cluster) = true ).
% ag4 didn't vote

happens( request0a( ag1, a, 							issue,	cluster ),	1).
happens( prepare1a( ag1, (0,2), 						issue,	cluster ),	2).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	issue,	cluster ), 	3).
happens( response1b(ag2, ( ag2, (0,1),a ), 		(0,2),	issue,	cluster ), 	4).
happens( response1b(ag3, ( ag3, (0,1),a ), 		(0,2),	issue,	cluster ), 	5).
happens( response1b(ag4, ( ag4, (0,0),null ), 	(0,2),	issue,	cluster ), 	6).
happens( leaveCluster(ag4,										cluster ),	7).
happens( prepare1a( ag1, (0,3), 						issue,	cluster ),	8).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,3),	issue,	cluster ), 	9).
happens( response1b(ag2, ( ag2, (0,1),a ), 		(0,3),	issue,	cluster ), 	10).
happens( response1b(ag3, ( ag3, (0,1),a ), 		(0,3),	issue,	cluster ), 	11).
% note that the loss of ag4 has (as expected) had no effect on the chosen-ness of a
happens( submit2a(	ag1, (0,3), a,						issue,	cluster ),	12).
% no change.