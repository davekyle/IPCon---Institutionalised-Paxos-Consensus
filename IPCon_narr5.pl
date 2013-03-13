%% Narrative to show addition of an agent when quorum is changed who doesn't agree
% this creates an obligation to revise
verbose :-
 false.

setOfAllAgents([ag1, ag2, ag3]).

%% Set original agent
initially( role_of(ag1, leader, any, issue, cluster) = true ).
initially( role_of(ag1, proposer, issue, cluster) = true).
initially( role_of(ag1, acceptor, 0, issue, cluster) = true ).
initially( voted(ag1, (0,1),a, 	issue, cluster) = true ).

happens( request0a( ag1, a, 0,							issue,	cluster ),	1).
happens( prepare1a( ag1, (0,2), 						issue,	cluster ),	2).
happens( response1b(ag1, ( ag1, (0,1),a ), 		(0,2), issue,	cluster ), 	3).
happens( submit2a(	ag1, (0,2), a,						issue,	cluster ),	4).
happens( vote2b( 	ag1, (0,2), a,						issue,	cluster ),	5).
happens( addRole(	ag1, ag2, acceptor,				0,	issue,	cluster ),	6).
happens( syncReq(	ag1, ag2, 	a, 					0,	issue,	cluster ),	7).
happens( syncAck(	ag2,		'no',					0,	issue,	cluster ),	8).
happens( addRole(	ag1, ag3, acceptor,				0,	issue,	cluster ),	9).
happens( syncReq(	ag1, ag3, 	a, 					0,	issue,	cluster ),	10).
happens( syncAck(	ag3,		'no',					0,	issue,	cluster ),	11).