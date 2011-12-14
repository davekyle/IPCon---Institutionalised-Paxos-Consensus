%% Narrative to show loss of multiple agents, and how consensus can be rebuilt (fragmentation)
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
initially( voted(ag1, (0,1),a,		issue, cluster) = true ).
initially( voted(ag2, (0,1),a, 		issue, cluster) = true ).
initially( voted(ag3, (0,1),a,		issue, cluster) = true ).
initially( voted(ag4, (0,0),null,		issue, cluster) = true ).
initially( voted(ag5, (0,0),null,		issue, cluster) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	issue, cluster) = true ).
initially( reportedVote(ag3, (0,1),a,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag4, (0,0),null,(0,1),		issue, cluster) = true ).
initially( reportedVote(ag5, (0,0),null,(0,1),		issue, cluster) = true ).


%happens( response1a(ag1, ( ag1, (0,1),a ), 		(0,2),	issue,	cluster ), 	3).
%happens( response1a(ag1, ( ag1, (0,1),a ), 		(0,4),	issue,	cluster ), 	19).
happens( leaveCluster( ag2,									cluster ),	1).
happens( leaveCluster( ag3,									cluster ),	2). 
happens( nothing ,																3). 
happens( revise(  ag1,										issue,	cluster ),	4).
