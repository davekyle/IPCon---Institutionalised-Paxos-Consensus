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

 
%%Round0%%
initially( voted(ag1, (0,0),null, 	issue, cluster) = true ).
initially( voted(ag2, (0,0),null,	issue, cluster) = true ).
initially( voted(ag3, (0,0),null,	issue, cluster) = true ).
initially( voted(ag4, (0,0),null,	issue, cluster) = true ).
initially( voted(ag5, (0,0),null,	issue, cluster) = true ).
%%Round1%%
initially( voted(ag1, (0,1),a, 	issue, cluster) = true ).
%%2%%
initially( voted(ag2, (0,2),b, 	issue, cluster) = true ).
initially( voted(ag3, (0,2),b, 	issue, cluster) = true ).
%%3%%
initially( voted(ag1, (0,3),a, 	issue, cluster) = true ).
initially( voted(ag4, (0,3),a, 	issue, cluster) = true ).
%%4%%
% initially( voted(ag2, (0,4),b, 	issue, cluster) = true ).
% initially( voted(ag3, (0,4),b, 	issue, cluster) = true ).
% initially( voted(ag5, (0,4),b, 	issue, cluster) = true ).
 
happens( request0a( ag1, a, 						issue,	cluster ),	1).
happens( request0a( ag2, b,							issue,	cluster ),	2).
happens( prepare1a( ag1, (0,10), 					issue,	cluster ),	3).
% Only reporting the most recent, yay
happens( response1b(ag1, ( ag1, (0,3),a ), 		(0,10), issue,	cluster ), 	4).
happens( response1b(ag2, ( ag2, (0,2),b ), 		(0,10), issue,	cluster ), 	5).
happens( response1b(ag3, ( ag3, (0,2),b ), 		(0,10), issue,	cluster ), 	6).
happens( response1b(ag4, ( ag4, (0,3),a ), 		(0,10), issue,	cluster ), 	7).
happens( response1b(ag5, ( ag5, (0,0),null ), 	(0,10), issue,	cluster ), 	8).
happens( submit2a(	ag1, (0,10),a,					issue,	cluster ),	9).
happens( vote2b( 	ag1, (0,10),b,					issue,	cluster ),	10).
happens( vote2b( 	ag2, (0,10),a,					issue,	cluster ),	11).
happens( vote2b( 	ag3, (0,10),a,					issue,	cluster ),	12).
happens( vote2b( 	ag4, (0,10),b,					issue,	cluster ),	13).
happens( vote2b( 	ag5, (0,10),a,					issue,	cluster ),	14).
happens( prepare1a(	ag1, (0,11),					issue,	cluster ),	15).
happens( leaveCluster(	ag1,								cluster ),	16).
happens( arrogateLeadership(	ag2,				issue,	cluster ),	17).
happens( addRole(	ag2, ag1, learner,			0,	issue,	cluster ),	18).
happens( revise(	ag2,							issue,	cluster ),	19).