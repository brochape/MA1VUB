:- module(find_heuristically, [find_heuristically/1, find_heuristically/2]).

:- use_module(helpers,[exam_list/1,simple_event_validation/1]).


%find_heuristically(+S)
find_heuristically(X):-
            exam_list(ExamsList),
            findall(X, simple_event_validation(X), AllEvents),
            events_for_exam(e1,AllEvents,[], Y),
            print(Y).
            %% find_heuristically(ExamsList, AllEvents, X, []).


events_for_exam(_, [], ExamEvents, ExamEvents).
events_for_exam(EID, [event(EID, RID, Day, Start)| AllEventsList] ,Events, ExamEvents):-
            events_for_exam(EID, AllEventsList,[event(EID, RID, Day, Start)|Events],ExamEvents).
events_for_exam(EID, [event(EID2, _, _, _)| AllEventsList] ,Events,ExamEvents):-
            EID  \= EID2,
            !,
            events_for_exam(EID, AllEventsList,Events,ExamEvents).


find_heuristically([EID, Exams], AllEvents, schedule([BestEvent|HeuristicEventsList]), CurrentEvents):-
            events_for_exam(EID, CurrentEvents, CurrentExamEvents),
            find_best_event(Events, HeuristicEventsList, CurrentExamEvents, BestEvent),
            !,
            delete_first(AllEvents, BestEvent, NewEvents),
            find_heuristically(Exams, NewEvents, )

find_heuristically(X,S).
            