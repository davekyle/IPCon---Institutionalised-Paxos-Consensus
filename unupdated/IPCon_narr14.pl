%% Less verbose experiment narration to show:
%% - merge (demonstrating ack/no and agent addition)
%% - revision
%% - fragmentation (demonstrating agent loss)
verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4]).
 
%%Make a leader
initially( role_of(ag1, leader, any, i, c1) = true ).
initially( role_of(ag1, proposer, any, i, c1) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, i, c1) = true ).
initially( role_of(ag2,acceptor, 0, i, c1) = true ).

initially( role_of(ag3,leader, any, i, c2) = true ).
initially( role_of(ag3, proposer, i, c2) = true ).
initially( role_of(ag3,acceptor, 0, i, c2) = true ).
initially( role_of(ag4,acceptor, 0, i, c2) = true ).
%%set previous votes - cluster1 has decided on a
initially( voted(ag1, (0,1),a,		i, c1) = true ).
initially( voted(ag2, (0,1),a, 		i, c1) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		i, c1) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	i, c1) = true ).
%%set previous votes - c2 has decided on b
initially( voted(ag3, (0,4),b,		i, c2) = true ).
initially( voted(ag4, (0,4),b, 		i, c2) = true ).
initially( reportedVote(ag3, (0,4),b,(0,4),		i, c2) = true ).
initially( reportedVote(ag4, (0,4),b,(0,4), 	i, c2) = true ).
%% 

%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	i,	c1 ), 	3).
%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,4),	i,	c1 ), 	19).

%% EG1 - merging clusters/agent addition; sync & revise resolves consensus
happens( leaveCluster([ag3, ag4],							c2),	1).
happens( addRole(ag1, [ag3, ag4], acceptor, 		0, 	i,	c1 ),	2).
happens( addRole(ag1, ag3, proposer, 			any,	i,	c1 ),	2).
happens( syncReq(	ag1, ag3, 	a, 					0,	i,	c1 ),	3).
happens( syncReq(	ag1, ag4, 	a, 					0,	i,	c1 ),	3).
happens( syncAck(	ag3,		a,					0,	i,	c1 ),	4).
happens( syncAck(	ag4,		no,					0,	i,	c1 ),	4).
happens( revise(	ag1,								i,	c1 ),	5).
happens( request0a( ag1, a, 							i,	c1 ),	6).
happens( request0a( ag3, c, 							i,	c1 ),	6).
happens( prepare1a( ag1, (1,1), 						i,	c1 ),	7).
% we only care about this revision, so no *need* to reply about others.
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	i,	c1 ), 	8).
happens( response1b(ag2, ( ag2, (1,0),null ), 	(1,1),	i,	c1 ), 	8).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,1),	i,	c1 ), 	8).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,1),	i,	c1 ), 	8).
happens( submit2a(	ag1, (1,1), a,						i,	c1 ),	9).
happens( vote2b( 	ag2, (1,1), a,						i,	c1 ),	10).
happens( prepare1a( ag1, (1,3), 						i,	c1 ),	11).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,3),	i,	c1 ), 	12).
happens( response1b(ag2, ( ag2, (1,1),a ),	 	(1,3),	i,	c1 ), 	12).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,3),	i,	c1 ), 	12).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,3),	i,	c1 ), 	12).
happens( submit2a(	ag1, (1,3), c,						i,	c1 ),	13).
happens( vote2b( 	ag1, (1,3), c,						i,	c1 ),	14).
happens( vote2b( 	ag3, (1,3), c,						i,	c1 ),	14).
happens( vote2b( 	ag4, (1,3), c,						i,	c1 ),	14).
%% chosen

%% EG2 - c1 fragmentation, leadership arrogation
happens(leaveCluster([ag2,ag3], 							c1 ),	15).
happens(arrogateLeadership( ag3, 						i,	c3 ),	16).
% we presume that if anyone objects, they can sort it out
happens( addRole(ag3, [ag2,ag3], acceptor, 	0,			i,	c3 ),	17).
happens( addRole(ag3, ag3, proposer, 			any,	i,	c3 ),	17).
happens( request0a( ag3, d, 							i,	c3 ),	18).
happens( prepare1a( ag3, (0,1), 						i,	c3 ),	19).
happens( response1b(ag3, ( ag3, (0,0),null ), 	(0,1),	i,	c3 ),	20).
happens( response1b(ag2, ( ag2, (0,0),null ), 	(0,1),	i,	c3 ),	20).
happens( submit2a(	ag3, (0,1), d,						i,	c3 ),	21).
happens( vote2b( 	ag3, (0,1), d,						i,	c3 ),	22).
happens( vote2b( 	ag2, (0,1), d,						i,	c3 ),	22).








