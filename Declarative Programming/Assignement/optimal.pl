
:- module(optimal,[is_optimal/1, find_optimal/1]).

:- use_module(is_valid,[is_valid/1]).
:- use_module(cost,[cost/2]).

%%%%%%%%%%%%
% Is_optimal
%%%%%%%%%%%%

is_optimal(X) :-
            findall([Schedule, Cost], 
                    (   is_valid(Schedule),
                        cost(Schedule, Cost)
                    ),
                    ScheduleList),
            find_opti(ScheduleList,OptimalSchedules),
            member(X, OptimalSchedules).

find_optimal(X):-
            is_optimal(X),
            !.%Only one needed

find_opti(ScheduleList,OptimalSchedules) :-
            sort(2,@=<,ScheduleList,Sorted),%Sort them per cost
            nth1(1,Sorted,First),
            nth1(2,First,Cost),% Takes the cost of the first elem (with the lowest cost)
            findall(Sched,member([Sched,Cost],Sorted),OptimalSchedules).%Makes a list of all the schedules that have that minimal cost
