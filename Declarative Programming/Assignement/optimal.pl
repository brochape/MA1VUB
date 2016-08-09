
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
            %% find_optimal_schedules(ScheduleList, OptimalSchedules, OptimalCost),
            find_opti(ScheduleList,OptimalSchedules),
            %% print(OptimalSchedules),
            member(X, OptimalSchedules).

find_optimal(X):-
            is_optimal(X),
            !.%Only one needed

find_opti(ScheduleList,OptimalSchedules) :-
            sort(2,@=<,ScheduleList,Sorted),%Sort them per cost
            nth1(1,Sorted,First),
            nth1(2,First,Cost),% Takes the cost of the first elem (with the lowest cost)
            findall(Sched,member([Sched,Cost],Sorted),OptimalSchedules).%Makes a list of all the schedules that have that minimal cost

%% first([Head|_],Head).



%%%%%%%%%%%%%%%%%%%%%%%%
%-----BACKUP OLD METHOD
%%%%%%%%%%%%%%%%%%%%%%%%
%% is_same_schedule(schedule([EventList1]),schedule([EventList2])):-
%%             permutation(EventList1,EventList2).%Same event in different order


%% %%Need to find multiple schedules since several schedules can have the same cost
%% find_optimal_schedules([(FirstSchedule,FirstCost)|ScheduleList], OptimalSchedules, OptimalCost):-
%%             find_optimal_schedules(ScheduleList, [FirstSchedule], OptimalSchedules, FirstCost, OptimalCost).


%% find_optimal_schedules([],OptimalSchedules, OptimalSchedules, OptimalCost, OptimalCost):- !.

%% %Same cost As the current optimal -> add to the list of optimals
%% find_optimal_schedules([(Schedule,ScheduleCost)|Schedules],Current, OptimalSchedules, ScheduleCost, OptimalCost):-
%%     find_optimal_schedules(Schedules,[Schedule|Current],OptimalSchedules,ScheduleCost, OptimalCost).

%% %Cost is less -> the current schedule won't be used
%% find_optimal_schedules([(_,ScheduleCost)|Schedules],Current,OptimalSchedules,Cost,OptimalCost):-
%%     Cost < ScheduleCost,
%%     find_optimal_schedules(Schedules,Current,OptimalSchedules,Cost,OptimalCost).
    
%% find_optimal_schedules([(Schedule,ScheduleCost)|Schedules],_, OptimalSchedules,Cost,OptimalCost):-
%%     ScheduleCost < Cost,
%%     find_optimal_schedules(Schedules,[Schedule],OptimalSchedules,ScheduleCost,OptimalCost).