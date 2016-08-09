:- module(sort,[sort_schedule/2]).

%Associates a value to an event to allow the sorting.
value_of_event(event(_,RID,Day,Start), Value) :-
            capacity(RID,RoomCapacity),
            Value is (1000000*Day + 1000*RoomCapacity + Start).

sort_schedule(schedule(Events),SortedSchedule):-
            map_list_to_pairs(value_of_event, Events, Pairs),
            keysort(Pairs, SortedPairs),
            pairs_values(SortedPairs, SortedSchedule).