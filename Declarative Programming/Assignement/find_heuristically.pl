:- module(find_heuristically, [ find_heuristically/1, 
                                find_heuristically/2]).

:- use_module(helpers,[         exam_list/1,
                                simple_event_validation/1,
                                delete_first/3]).
:- use_module(is_valid, [       is_valid/1,
                                are_events_valid/1]).
:- use_module(cost, [           cost/2]).


%find_heuristically(+S)
find_heuristically(X):-
            exam_list(ExamsList),
            findall(ValidEvent, simple_event_validation(ValidEvent), AllEvents),
            find_heuristically(ExamsList,ExamsList, AllEvents, [], X,-1).

find_heuristically(_,_,_,_,_, Deadline):-
            not(Deadline is -1),
            !,
            get_time(CurrentTime),
            CurrentTime >= Deadline,
            fail.%false would lead to continuing in the next predicate-> we need to kill it

find_heuristically([EID| Exams], UncheckedEvents, CurrentEventsTillNow, [BestEvent|HeuristicEventsList], FinalExamList, Deadline):-
            events_for_exam(EID, UncheckedEvents, [],CurrentExamEvents),
            find_best_event(Events, CurrentEventsTillNow, CurrentExamEvents, BestEvent),
            !,
            delete_first(UncheckedEvents, BestEvent, NewEvents),
            find_heuristically(Exams, NewEvents,  [BestEvent|CurrentEventsTillNow], HeuristicEventsList, FinalExamList, Deadline).
find_heuristically([],UncheckedEvents, CurrentEventsTillNow, HeuristicEventsList,  FinalExamList, Deadline):-
            not(is_valid(schedule(CurrentEventsTillNow))),
            !,
            correct_schedule(FinalExamList, FinalExamList, EventsUntilNow, UncheckedEvents, HeuristicEventsList).
find_heuristically([], _, schedule([]), _):- !.


find_heuristically(X,MaxDuration):-
            get_time(StartTime),
            TimeMax is StartTime + MaxDuration,
            exam_list(ExamsList),
            findall(X, simple_event_validation(X), AllEvents),
            find_heuristically(ExamsList, AllEvents, X, [], FinalExamList,TimeMax).

events_for_exam(_, [], ExamEvents, ExamEvents).
events_for_exam(EID, [event(EID, RID, Day, Start)| AllEventsList] ,Events, ExamEvents):-
            events_for_exam(EID, AllEventsList,[event(EID, RID, Day, Start)|Events],ExamEvents).
events_for_exam(EID, [event(EID2, _, _, _)| AllEventsList] ,Events,ExamEvents):-
            EID  \= EID2,
            !,
            events_for_exam(EID, AllEventsList,Events,ExamEvents).

% find_best_event(+ExamID, +EventsUntilNow, +UncheckedEventsForEID, -BestEvent)
find_best_event(Exam, EventsUntilNow, UncheckedEvents, BestEvent) :-
            find_best_event(Exam, EventsUntilNow, UncheckedEvents, inf, nil, BestEvent),
            BestEvent \= nil.

find_best_event(_, _, [], _, BestEvent, BestEvent).

find_best_event(EID, EventsUntilNow, [Event|Events], BestCost, _, BestEvent) :-
            cost(schedule([Event|EventsUntilNow]), Cost),
            are_events_valid([Event|EventsUntilNow]),
            (Cost < BestCost ; BestCost == -1),
            find_best_event(EID, EventsUntilNow, Events, Cost, Event, BestEvent).

find_best_event(EID, EventsUntilNow, [_|Events], BestCost, AccBestEvent, BestEvent) :-
            find_best_event(EID, EventsUntilNow, Events, BestCost, AccBestEvent, BestEvent).


% The schedule is fixed.
correct_schedule(_, FinalExamList, ValidEvents, _, ValidEvents) :-
            is_valid(schedule(ValidEvents)).
% Changed it but still invalid, tries again
correct_schedule([], FinalExamList, Events, UncheckedEvents, ValidEvents) :-
            not(is_valid(schedule(Events))),
            !,
            correct_schedule(FinalExamList, FinalExamList, Events, UncheckedEvents, ValidEvents).
% Replace one exam  at a time
correct_schedule([EID|Exams], FinalExamList,  Events, UncheckedEvents, ValidEvents) :-
            delete(Events, event(EID, _, _, _), NewEvents),
            events_for_exam(UncheckedEvents, EID, [], UncheckedEventsForEID),
            find_best_event(EID, NewEvents, UncheckedEventsForEID, BestEvent),
            delete(UncheckedEvents, BestEvent, NewUncheckedEvents),
            correct_schedule(Exams, FinalExamList, [BestEvent|NewEvents], NewUncheckedEvents, ValidEvents).


            

%% AllEvents,CurrentEventsTillNow ->             UncheckedEvents
%% 