:- module(pretty_print,[pretty_print/1, pretty_print/2]).


:- use_module(sort_schedule, [sort_schedule/2]).
:- use_module(helpers,[end/3, attends_exam/2]).

%%===============PRETTY_PRINT=================
%%   PRINTS A HUMAN-READABLE SORTED SCHEDULE
%%============================================




%%pretty_print(+S)
pretty_print(Schedule):-
            writef("=======================\n       Schedule\n=======================\n"),
            sort_schedule(Schedule,SortedEvents),
            print_next(SortedEvents,0,0),
            !.

%%pretty_print(+SID,+S)
pretty_print(SID, Schedule):-
            writef("=======================\n       Schedule\n=======================\n"),
            sort_schedule(Schedule,SortedEvents),
            get_events_for_student(SID, SortedEvents, EventsForSID),
            print_next(EventsForSID,0,0).

get_events_for_student(_, [], []).
get_events_for_student(SID,[event(EID, _, _, _)|Events], StudentEvents):-
            has_exam(CID,EID),
            not(follows(SID, CID)),
            get_events_for_student(SID, Events, StudentEvents).

get_events_for_student(SID,[Event|Events],[Event|StudentEvents]):-
            get_events_for_student(SID, Events, StudentEvents).


%%%%%%%%%%%%
%%% COMPLETE SCHEDULE
%%%%%%%%%%%%

print_next([],_,_) :-!.
%% Same day, same room
print_next([event(EID,PreviousRoom,PreviousDay,Start)|OtherEvents],PreviousDay,PreviousRoom):-
            print_event(EID,PreviousRoom,Start),
            print_next(OtherEvents,PreviousDay,PreviousRoom).

%%Same day, different room
print_next([event(EID,RID,PreviousDay,Start)|OtherEvents],PreviousDay,_):-
            print_room(RID),
            print_event(EID,RID,Start),
            print_next(OtherEvents,PreviousDay,RID).

%%Otherwise
print_next([event(EID,RID,Day,Start)|OtherEvents],_,_):-
            print_day(Day),
            print_room(RID),
            print_event(EID,RID,Start),
            print_next(OtherEvents,Day,0).

print_day(Day):-
            format("\n Day ~I \n========\n\n",[Day]).

print_room(RoomID):-
            room(RoomID,RoomName),
            format('~s :\n',[RoomName]).

print_event(ExamID,_,Start):-
            exam(ExamID,ExamName),
            end(ExamID,Start,End),
            format('~p:00 - ~p:00 : ~s\n',[Start,End,ExamName]).