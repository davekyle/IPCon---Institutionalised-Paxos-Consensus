%% Experiment narration to show:
%% - temp failure within assumptions 
%% - merge
%% - ack/no
%% - revision
%% - agent exit
%% - fragmentation
%% - temp failure of multiple agents averted by learners
verbose :-
 false.
 
setOfAllAgents([ag1, ag2, ag3, ag4, ag5, ag6, ag7, ag8]).
 
%%Make a leader
initially( role_of(ag1, leader, any, i, c1) = true ).
initially( role_of(ag1, proposer, i, c1) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, i, c1) = true ).
initially( role_of(ag2,acceptor, 0, i, c1) = true ).
initially( role_of(ag3,acceptor, 0, i, c1) = true ).
initially( role_of(ag4,acceptor, 0, i, c1) = true ).

initially( role_of(ag5,leader, any, i, c2) = true ).
initially( role_of(ag5, proposer, i, c2) = true ).
initially( role_of(ag5,acceptor, 0, i, c2) = true ).
initially( role_of(ag6,acceptor, 0, i, c2) = true ).
initially( role_of(ag7,acceptor, 0, i, c2) = true ).
initially( role_of(ag8,acceptor, 0, i, c2) = true ).
%%set previous votes - cluster1 has decided on a
initially( voted(ag1, (0,1),a,		i, c1) = true ).
initially( voted(ag2, (0,1),a, 		i, c1) = true ).
initially( voted(ag3, (0,1),a,		i, c1) = true ).
initially( reportedVote(ag1, (0,1),a,(0,1),		i, c1) = true ).
initially( reportedVote(ag2, (0,1),a,(0,1), 	i, c1) = true ).
initially( reportedVote(ag3, (0,1),a,(0,1),		i, c1) = true ).
% ag4 hasn't voted
%%set previous votes - c2 has decided on b
initially( voted(ag5, (0,4),b,		i, c2) = true ).
initially( voted(ag6, (0,4),b, 		i, c2) = true ).
initially( voted(ag7, (0,4),b,		i, c2) = true ).
initially( voted(ag8, (0,4),b,		i, c2) = true ).
initially( reportedVote(ag5, (0,4),b,(0,4),		i, c2) = true ).
initially( reportedVote(ag6, (0,4),b,(0,4), 	i, c2) = true ).
initially( reportedVote(ag7, (0,4),b,(0,4),		i, c2) = true ).
initially( reportedVote(ag8, (0,4),b,(0,4),		i, c2) = true ).
%% 
initially( proposed( a, 0,			i, c1 ) = true ).

%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	i,	c1 ), 	3).
%happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,4),	i,	c1 ), 	19).

%% EG1 - temporary role failure of acceptor ag3; Paxos shows no problem.
happens( prepare1a( ag1, (0,2), 						i,	c1 ),	1).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2),	i,	c1 ), 	2).
happens( response1b(ag2, ( ag2, (0,1),a ), 		(0,2),	i,	c1 ), 	2).
happens( response1b(ag4, ( ag4, (0,0),null ), 	(0,2),	i,	c1 ), 	2).
happens( submit2a(	ag1, (0,2), a,						i,	c1 ),	3).
happens( vote2b( 	ag1, (0,2), a,						i,	c1 ),	4).
happens( vote2b( 	ag2, (0,2), a,						i,	c1 ),	4).
happens( vote2b( 	ag3, (0,2), a,						i,	c1 ),	4).
happens( vote2b( 	ag4, (0,2), a,						i,	c1 ),	4).


%% EG2 - merging clusters/agent addition; sync & revise resolves consensus
happens( leaveCluster([ag5, ag6, ag7, ag8],						c2),	5).
happens( addRole(ag1, [ag5, ag6, ag7, ag8], acceptor, 0, i,	c1 ),	6).
happens( addRole(ag1, ag5, proposer, 			any,	i,	c1 ),	6).
happens( syncReq(	ag1, ag5, 	a, 					0,	i,	c1 ),	7).
happens( syncReq(	ag1, ag6, 	a, 					0,	i,	c1 ),	7).
happens( syncReq(	ag1, ag7, 	a, 					0,	i,	c1 ),	7).
happens( syncReq(	ag1, ag8, 	a, 					0,	i,	c1 ),	7).
happens( syncAck(	ag5,		a,					0,	i,	c1 ),	8).
happens( syncAck(	ag6,		a,					0,	i,	c1 ),	8).
happens( syncAck(	ag7,		no,					0,	i,	c1 ),	9).
happens( revise(	ag1,								i,	c1 ),	10).
happens( request0a( ag1, a, 1,							i,	c1 ),	11).
happens( request0a( ag5, c, 1,							i,	c1 ),	11).
happens( prepare1a( ag1, (1,1), 						i,	c1 ),	12).
% we only care about this revision, so no *need* to reply about others.
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag2, ( ag2, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag3, ( ag3, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag5, ( ag5, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag6, ( ag6, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag7, ( ag7, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( response1b(ag8, ( ag8, (1,0),null ), 	(1,1),	i,	c1 ), 	13).
happens( submit2a(	ag1, (1,1), a,						i,	c1 ),	14).
happens( vote2b( 	ag2, (1,1), a,						i,	c1 ),	15).
happens( vote2b( 	ag3, (1,1), a,						i,	c1 ),	15).
happens( prepare1a( ag1, (1,3), 						i,	c1 ),	16).
happens( response1b(ag1, ( ag1, (1,0),null ), 	(1,3),	i,	c1 ), 	17).
happens( response1b(ag2, ( ag2, (1,1),a ),	 	(1,3),	i,	c1 ), 	17).
happens( response1b(ag3, ( ag3, (1,1),a ), 		(1,3),	i,	c1 ), 	17).
happens( response1b(ag4, ( ag4, (1,0),null ), 	(1,3),	i,	c1 ), 	17).
happens( response1b(ag5, ( ag5, (1,0),null ), 	(1,3),	i,	c1 ), 	17).
happens( response1b(ag6, ( ag6, (1,0),null ), 	(1,3),	i,	c1 ), 	17).
happens( response1b(ag7, ( ag7, (1,0),null ),	(1,3),	i,	c1 ), 	17).
happens( response1b(ag8, ( ag8, (1,0),null ), 	(1,3),	i,	c1 ), 	17).
happens( submit2a(	ag1, (1,3), c,						i,	c1 ),	18).
happens( vote2b( 	ag1, (1,3), c,						i,	c1 ),	19).
happens( vote2b( 	ag3, (1,3), c,						i,	c1 ),	19).
happens( vote2b( 	ag4, (1,3), c,						i,	c1 ),	19).
happens( vote2b( 	ag5, (1,3), c,						i,	c1 ),	19).
happens( vote2b( 	ag6, (1,3), c,						i,	c1 ),	19).
happens( vote2b( 	ag8, (1,3), c,						i,	c1 ),	19).
%% chosen

%% EG3 - an agent leaves; no problem for consensus
happens( leaveCluster(ag8,										c1 ),	20).

%% EG4 - c1 fragmentation, leadership arrogation
happens(leaveCluster([ag3, ag4, ag5,ag6,ag7], 					c1 ),	21).
happens(arrogateLeadership( ag5, 						i,	c3 ),	22).
% we presume that if anyone objects, they can sort it out
happens( addRole(ag5, [ag3, ag4, ag5, ag6, ag7], acceptor, 	0,	i,	c3 ),	23).
happens( addRole(ag5, ag5, proposer, 			any,	i,	c3 ),	23).
happens( request0a( ag5, d, 0,							i,	c3 ),	24).
happens( prepare1a( ag5, (0,1), 						i,	c3 ),	25).
happens( response1b(ag3, ( ag3, (0,0),null ), 	(0,1),	i,	c3 ), 26).
happens( response1b(ag4, ( ag4, (0,0),null ), 	(0,1),	i,	c3 ), 26).
happens( response1b(ag5, ( ag5, (0,0),null ), 	(0,1),	i,	c3 ), 26).
happens( response1b(ag6, ( ag6, (0,0),null ), 	(0,1),	i,	c3 ), 26).
happens( response1b(ag7, ( ag7, (0,0),null ),	(0,1),	i,	c3 ), 26).
happens( submit2a(	ag5, (0,1), d,						i,	c3 ),	27).
happens( vote2b( 	ag3, (0,1), d,						i,	c3 ),	28).
happens( vote2b( 	ag4, (0,1), d,						i,	c3 ),	28).
happens( vote2b( 	ag5, (0,1), d,						i,	c3 ),	28).
happens( vote2b( 	ag6, (0,1), d,						i,	c3 ),	28).
happens( vote2b( 	ag7, (0,1), d,						i,	c3 ),	28).

%% EG5 - obligations to reply
%%%happens( addRole(ag5, ag6, learner, 			0,		i,	c3 ), 29).
happens( request0a( ag5, d, 0,							i,	c3 ),	29).
happens( prepare1a( ag5, (0,2), 						i,	c3 ),	30).
%% obligations to reply show here
happens( response1b(ag3, ( ag3, (0,1),d ), 	(0,2),		i,	c3 ), 31).
happens( response1b(ag4, ( ag4, (0,1),d ), 	(0,2),		i,	c3 ), 31).
happens( response1b(ag5, ( ag5, (0,1),d ), 	(0,2),		i,	c3 ), 31).
happens( response1b(ag6, ( ag6, (0,1),d ), 	(0,2),		i,	c3 ), 31).
%% we can still see obl to reply for ag7 here.
happens( response1b(ag7, ( ag7, (0,1),d ), 	(0,2),		i,	c3 ), 32).
happens( submit2a(	ag5, (0,2), d,						i,	c3 ),	33).








