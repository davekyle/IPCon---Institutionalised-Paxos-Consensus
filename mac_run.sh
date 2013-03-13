#!/opt/local/bin/yap -L --
# .

:- initialization(main).

main :-
	yap_flag(language, iso),
	consult('IPCon_PowPerOblMod.pl'),
	consult('IPCon_narr19.pl'),
	consult('IPCon_utils.pl'),
	findall(Time, Action^happens(Action,Time), Times),
	reverse(Times, [], Reversed), Reversed = [N|T],
	d(N),
	nl.
