:- module(find_heuristically, [ find_heuristically/1, 
                                find_heuristically/2]).

:- use_module(helpers,[         exam_list/1,
                                simple_event_validation_heur/1,
                                delete_first/3]).
:- use_module(is_valid, [       is_valid/1,
                                are_events_valid/1]).
:- use_module(cost, [           cost/2]).


%find_heuristically(-S)
%Heuristically finds a schedule
find_heuristically(X):-
            exam_list(ExamsList),
            findall(ValidEvent, simple_event_validation_heur(ValidEvent), AllEvents),
            find_heuristically(ExamsList, AllEvents, [], X, ExamsList, -1).

%find_heuristically(-S,+T)
%Heuristically finds a schedule within a certain amount of time
find_heuristically(X,MaxDuration):-
            get_time(StartTime),
            exam_list(ExamsList),
            TimeMax is StartTime + MaxDuration,
            findall(ValidEvent, simple_event_validation(ValidEvent), AllEvents),
            find_heuristically(ExamsList, AllEvents, [], X, ExamsList,TimeMax),
            !.

%First one to check if the time has passed
find_heuristically(_,_,_,_,_, Deadline):-
            not(Deadline is -1),
            get_time(CurrentTime),
            CurrentTime >= Deadline,
            !,
            fail.%false would lead to continuing in the next predicate-> we need to kill it

find_heuristically([EID| Exams], UncheckedEvents, CurrentEventsTillNow, [BestEvent|HeuristicEventsList], FinalExamList, Deadline):-
            events_for_exam( UncheckedEvents,EID,CurrentExamEvents),
            find_best_event(Events, CurrentEventsTillNow, CurrentExamEvents, BestEvent),
            !,
            delete_first(BestEvent, UncheckedEvents, NewEvents),
            find_heuristically(Exams, NewEvents,  [BestEvent|CurrentEventsTillNow], HeuristicEventsList, FinalExamList, Deadline).

find_heuristically([],UncheckedEvents, CurrentEventsTillNow, HeuristicEventsList,  FinalExamList, Deadline):-
            not(is_valid(schedule(CurrentEventsTillNow))),
            !,
            correct_schedule(FinalExamList, FinalExamList, EventsUntilNow, UncheckedEvents, HeuristicEventsList).
%If we placed every exam, done
find_heuristically([], _, _, [], _,_):- !.


%% events_for_exam(+EID, +AllEvents, -EventsForExam)
% Gets all the events for a particular event

events_for_exam(AllEvents, EID, EventsForExam):-
    findall(event(EID, RID, Day, Start), member(event(EID, RID, Day, Start), AllEvents), EventsForExam).

% find_best_event(+ExamID, +EventsUntilNow, +PendingEventsForEID, -BestEvent)
% Finds the event that would give the lowest cost
find_best_event(Exam, EventsUntilNow, PendingEventsForEID, BestEvent) :-
            find_best_event(Exam, EventsUntilNow, PendingEventsForEID, -1, -1, BestEvent),
            not(BestEvent is -1).

find_best_event(_, _, [], _, BestEvent, BestEvent).

find_best_event(EID, EventsUntilNow, [Event|Events], LowestCost, _, BestEvent) :-
            cost(schedule([Event|EventsUntilNow]), Cost),
            are_events_valid([Event|EventsUntilNow]),
            (Cost < LowestCost ; LowestCost == -1),
            !,
            find_best_event(EID, EventsUntilNow, Events, Cost, Event, BestEvent).

find_best_event(EID, EventsUntilNow, [_|Events], LowestCost, AccBestEvent, BestEvent) :-
            find_best_event(EID, EventsUntilNow, Events, LowestCost, AccBestEvent, BestEvent).



%correct_schedule(+ExamList, +FinalExamList, +Events, +PendingEvents, -aValidEvents)
% Takes a schedule that in't valid and replaces events until it is.
% The schedule is fixed.
correct_schedule(_, _, ValidEvents, _, ValidEvents) :-
            is_valid(schedule(ValidEvents)).
% Changed it but still invalid, tries again
correct_schedule([], FinalExamList, Events, PendingEvents, ValidEvents) :-
            not(is_valid(schedule(Events))),
            !,
            correct_schedule(FinalExamList, FinalExamList, Events, PendingEvents, ValidEvents).
% Replace one exam  at a time
correct_schedule([EID|Exams], FinalExamList,  Events, PendingEvents, ValidEvents) :-
            delete(Events, event(EID, _, _, _), NewEvents),
            events_for_exam( PendingEvents, EID, PendingEventsForEID),
            find_best_event(EID, NewEvents, PendingEventsForEID, BestEvent),
            delete(PendingEvents, BestEvent, NewPendingEvents),
            correct_schedule(Exams, FinalExamList, [BestEvent|NewEvents], NewPendingEvents, ValidEvents).


    