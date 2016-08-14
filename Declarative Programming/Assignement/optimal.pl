
:- module(optimal,[is_optimal/1, find_optimal/1]).

:- use_module(is_valid,[is_valid/1]).
:- use_module(cost,[cost/2]).

%%%%%%%%%%%%
% Is_optimal
%%%%%%%%%%%%

%is_optimal(?S)
%%Checks if an event

is_optimal(X) :-
            findall([Schedule, Cost], 
                    (   is_valid(Schedule),
                        cost(Schedule, Cost)
                    ),
                    ScheduleList),
            find_opti(ScheduleList,OptimalSchedules),
            member(X, OptimalSchedules).

%%find_optimal(-S)
%%Finds one(thanks to the cut) optimal schedule
find_optimal(X):-
            is_optimal(X),
            !.
%%find_opti(+Ss, -OptimalSchedules)
%%Sorts the (schedule,cost) per cost, takes the cost of the first one(with the lowest cost) and finds all the schedules with the same cost.
find_opti(ScheduleList,OptimalSchedules) :-
            sort(2,@=<,ScheduleList,Sorted),
            nth1(1,Sorted,First),
            nth1(2,First,Cost),
            findall(Sched,member([Sched,Cost],Sorted),OptimalSchedules).
