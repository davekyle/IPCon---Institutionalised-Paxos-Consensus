verbose :-
 false.

setOfAllAgents([ag1, ag2, ag3, ag4, ag5]).
 
%%Make a leader
initially( role_of(ag1, leader, any, issue, cluster) = true ).	% leader agent
initially( role_of(ag1, proposer, issue, cluster) = true).
initially( role_of(ag2, proposer, issue, cluster) = true).
%%Make everyone an acceptor in everything
initially( role_of(ag1,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag2,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag3,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag4,acceptor, 0, issue, cluster) = true ).
initially( role_of(ag5,acceptor, 0, issue, cluster) = true ).
%initially( role_of(ag5,learner, 0, issue, cluster) = true ).
initially( role_of(ag1,acceptor, 1, issue, cluster) = true ).
initially( role_of(ag2,acceptor, 1, issue, cluster) = true ).
initially( role_of(ag3,acceptor, 1, issue, cluster) = true ).
initially( role_of(ag4,acceptor, 1, issue, cluster) = true ).
initially( role_of(ag5,acceptor, 1, issue, cluster) = true ).
 
%set previous votes
initially( voted(ag1, (0,0),null, 	issue, cluster) = true ).
initially( voted(ag2, (0,1),z, 		issue, cluster) = true ).
initially( voted(ag3, (0,0),null,	issue, cluster) = true ).
initially( voted(ag4, (0,0),null,	issue, cluster) = true ).
initially( voted(ag5, (0,0),null,	issue, cluster) = true ).
% checking for quorum-choosing
initially( voted(ag2, (1,0),4,		issue, cluster) = true ).
initially( voted(ag3, (1,0),4,		issue, cluster) = true ).
initially( voted(ag4, (1,1),5,		issue, cluster) = true ).

 
happens( request0a( ag1, 3, 1,						issue, cluster ),	1).
happens( prepare1a( ag1, (1,10), 					issue, cluster ),	2).
happens( response1b(ag1, ( ag1, (0,0),null ), 	(1,10), issue, cluster ), 	3).
happens( response1b(ag2, ( ag2, (1,0),4 ), 		(1,10), issue, cluster ), 	4).
happens( response1b(ag3, ( ag3, (1,0),4 ),		(1,10), issue, cluster ), 	5).
%happens( response1b(ag4, ( ag4, (0,0),null ), 	(1,10), issue, cluster ), 	6).
happens( response1b(ag4, ( ag4, (1,1),5 ),	 	(1,10), issue, cluster ), 	6).
happens( response1b(ag5, ( ag5, (0,0),null ), 	(1,10), issue, cluster ), 	7).
happens( submit2a(	ag1, (1,10),3,					issue, cluster ),	8).
happens( vote2b( 	ag1, (1,10),3,					issue, cluster ),	9).
happens( vote2b( 	ag2, (1,10),3,					issue, cluster ),	10).
happens( vote2b( 	ag3, (1,10),4,					issue, cluster ),	11).
happens( vote2b( 	ag4, (1,10),3,					issue, cluster ),	12).
happens( vote2b( 	ag5, (1,10),3,					issue, cluster ),	13).