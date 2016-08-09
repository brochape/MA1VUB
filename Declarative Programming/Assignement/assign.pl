%% consult('small_instance.pl').

:- use_module('is_valid.pl').
:- use_module('cost.pl').
:- use_module('pretty_print.pl').
:- use_module('optimal.pl').










%% pretty_print(schedule([event(e1, r1, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
%% sort_schedule(schedule([event(e1, r2, 1, 10), event(e2, r2, 5, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10), event(e5, r2, 5, 15)])).
%% price(2,Price) :- Price is 1.
%% price(4,Price) :- Price is 3.
%% price(6,Price) :- Price is 2.



%%%%%%%%%%%%%%%%%%%%%
%% find_heuristically
%%%%%%%%%%%%%%%%%%%%%


%ààcost(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10.5), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]),Y).

%%is_valid(schedule([event(e3, r1, 1, 10), event(e4, r2, 2, 10)])))]))

%event(EID,RID,Day,Start)
%% are_time_overlapping(event(e1, r1, 3, 10), event(e2, r2, 3, 10)).
%no_overlapping_for_event(event(e3, r1, 1, 10), event(e4, r2, 2, 10)).
%pretty_print(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10.5), event(e4, r2, 4, 10), event(e5, r2, 5, 10)])).



%% violates_b2b_loop(Event, [Event2|Events], ):-
