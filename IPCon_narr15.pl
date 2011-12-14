%% EVEN less verbose experiment narration to show:
%% - fragmentation (demonstrating agent loss)
%% - revision
%% - merge (demonstrating ack/no and agent addition)


verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4, ag5, ag6, ag7, ag8]).
 
%%Make a leader
initially( role_of(ag6, leader, any, i, c1) = true ).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, i, c1) = true ).
initially( role_of(ag2,proposer, any, i, c1) = true).
initially( role_of(ag2,acceptor, 0, i, c1) = true ).
initially( role_of(ag3,acceptor, 0, i, c1) = true ).
initially( role_of(ag4,acceptor, 0, i, c1) = true ).
initially( role_of(ag5,acceptor, 0, i, c1) = true ).
initially( role_of(ag6,acceptor, 0, i, c1) = true ).

%%set previous votes - c11 has decided on a
initially( voted(ag1, (0,1),a,		i, c1) = true ).
initially( voted(ag2, (0,1),a, 		i, c1) = true ).
initially( voted(ag3, (0,1),a,		i, c1) = true ).
initially( voted(ag4, (0,1),a, 		i, c1) = true ).
initially( voted(ag5, (0,1),a,		i, c1) = true ).
initially( voted(ag6, (0,1),a,		i, c1) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		i, c1) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	i, c1) = true ).
initially( reportedVote(ag3, (0,1),a,(0,1),		i, c1) = true ).
initially( reportedVote(ag4, (0,1),a,(0,1), 	i, c1) = true ).
initially( reportedVote(ag5, (0,1),a,(0,1),		i, c1) = true ).
initially( reportedVote(ag6, (0,1),a,(0,1),		i, c1) = true ).

%% 

%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	i,	c1 ), 	3).
%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,4),	i,	c1 ), 	19).

%% EG1 - merging c1s/agent addition; sync & revise resolves consensus
happens( leaveCluster([ag4, ag5, ag6],						c1 ),	1).
happens( arrogateLeadership( ag1, 						i,	c1 ),	2).
happens( revise(	ag1,								i,	c1 ),	3).
happens( request0a( ag2, b, 							i,	c1 ),	4).
happens( prepare1a( ag1, (1,1), 						i,	c1 ),	5).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	i,	c1 ), 	6).
happens( response1b(ag2, ( ag2, (1,0),null ), 	(1,1),	i,	c1 ), 	6).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,1),	i,	c1 ), 	6).
happens( submit2a(	ag1, (1,1), b,						i,	c1 ),	7).
happens( vote2b( 	ag1, (1,1), b,						i,	c1 ),	8).
happens( vote2b( 	ag2, (1,1), b,						i,	c1 ),	8).
happens( vote2b( 	ag3, (1,1), b,						i,	c1 ),	8).


happens( addRole(	ag1, [ag7,ag8], acceptor, 		1,	i,	c1 ),	9).
happens( syncReq(	ag1, ag7, 	b, 					1,	i,	c1 ),	10).
happens( syncReq(	ag1, ag8, 	b, 					1,	i,	c1 ),	10).
happens( syncAck(	ag7, 		b, 					1,	i,	c1 ),	11).
happens( syncAck(	ag8, 		no, 				1,	i,	c1 ),	11).
happens( revise(	ag1,								i,	c1 ),	12).
happens( request0a( ag2, c, 							i,	c1 ),	13).
happens( prepare1a( ag1, (2,1), 						i,	c1 ),	14).
% we only care about this revision, so no *need* to reply about others.
happens( response1b(ag1, ( ag1, (2,0),null ), 	(2,1),	i,	c1 ), 	15).
happens( response1b(ag2, ( ag2, (2,0),null ), 	(2,1),	i,	c1 ), 	15).
happens( response1b(ag3, ( ag3, (2,0),null ), 	(2,1),	i,	c1 ), 	15).
happens( response1b(ag7, ( ag7, (2,0),null ), 	(2,1),	i,	c1 ), 	15).
happens( response1b(ag8, ( ag8, (2,0),null ), 	(2,1),	i,	c1 ), 	15).
happens( submit2a(	ag1, (2,1), c,						i,	c1 ),	16).
happens( vote2b( 	ag1, (2,1), c,						i,	c1 ),	17).
happens( vote2b( 	ag2, (2,1), c,						i,	c1 ),	17).
happens( vote2b( 	ag8, (2,1), c,						i,	c1 ),	17).


