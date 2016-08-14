:- module(is_valid,[    is_valid/1,
                        are_events_valid/1]).

:- use_module(helpers, [takes_exam/2,
                        end/3, 
                        exam_list/1, 
                        during_session/1, 
                        simple_event_validation/1, 
                        can_students_fit/2,
                        number_of_students/2,
                        is_room_available/1]).


%are_time_overlapping(+E1, +E2)
%Checks if 2 events are overlapping
are_time_overlapping(event(Exam1, _, Day, Start1), event(_, _, Day, Start2)) :- 
            end(Exam1, Start1, End1),
            Start1=<Start2,
            Start2<End1.

are_time_overlapping(event(_, _, Day, Start1), event(Exam2, _, Day, Start2))  :- 
            end(Exam2, Start2,  End2),
            Start1 > Start2,
            Start1 <End2.

%no_overlapping_for_event(+Event,+RestEvents)
% Checks if there is no overlapping between an event and the rest of the events
no_overlapping_for_event(_,[]):- !.
no_overlapping_for_event(event(EID1,RID1,Day1,Start1),[event(EID2,RID2,Day2,Start2)|Rest]) :-
            no_students_teacher_overlap(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2)),% Occupants check
            not(%Room check
                    (are_time_overlapping(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2)),
                    RID1 = RID2)
                ),
            no_overlapping_for_event(event(EID1,RID1,Day1,Start1),Rest).

%% Checks if a list of events is valid
are_events_valid([]) :- !.
are_events_valid([event(EID,RID,Day,Start)|Events]) :-
            simple_event_validation(event(EID,RID,Day,Start)),
            no_overlapping_for_event(event(EID,RID,Day,Start),Events),
            are_events_valid(Events).

%%create_schedule(-X, +ExamsList)
%%Creates a schedule
create_schedule(X, ExamsList) :-
            create_schedule(X, [], ExamsList).
create_schedule(schedule([]), _, []) :- !.
create_schedule(schedule([event(CurrentExam, Room, Day, Start) | Current]), ExistingEvents, [CurrentExam|RestExams]) :-
            has_exam(_, CurrentExam),
            can_students_fit(CurrentExam, Room),
            is_room_available(event(CurrentExam, Room, Day, Start)),
            during_session(Day),
            no_overlapping_for_event(event(CurrentExam,Room,Day,Start), ExistingEvents),
            create_schedule(schedule(Current), [event(CurrentExam, Room, Day, Start) | ExistingEvents], RestExams).

%% Checks if a list has no duplicate
%% Source : http://stackoverflow.com/questions/9005953/testing-whether-a-list-represents-a-set-with-no-duplicates
no_duplicates(L) :-
            setof(X, member(X, L), Set), 
            length(Set, Len), 
            length(L, Len).

%% Checks if 2 exams don't have students or the teacher in common
no_common_teacher_or_student(EID1,EID2) :-
            has_exam(Course1,EID1),
            teaches(Teacher1,Course1),
            has_exam(Course2,EID2),
            teaches(Teacher2, Course2),
            findall(Student, (takes_exam(Student,EID1) ; takes_exam(Student, EID2)), Lst),%Creates a list with all the students from both courses
            Teacher1 \= Teacher2,
            no_duplicates(Lst).%Checks if the students only take one exam or the other, but not both

%%Check if 2 events don't overlap while having teachers in common 
no_students_teacher_overlap(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2)) :-
            not(are_time_overlapping(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2))).
no_students_teacher_overlap(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2)) :-
            are_time_overlapping(event(EID1,RID1,Day1,Start1),event(EID2,RID2,Day2,Start2)),
            no_common_teacher_or_student(EID1,EID2).







% Verifies that an exam only happens once but still happens by removing the exams that are listed in events in the exam list
% If an exam is listed twice or not listed, we won't reach the state with 2 empty lists (either by reaching false in selectchk or by having a non-empty list of exams)
exams_happen_once_only([],[]).
exams_happen_once_only([event(EID,_,_,_)|EventList], ExamList):-
            selectchk(EID, ExamList, NewExamList),
            exams_happen_once_only(EventList, NewExamList).

%% is_valid(?S)

is_valid(X):- is_valid_check(X).

%%Allows to factorize the exams_happen_once_only
check_validity(EventList,ExamList):-
            exams_happen_once_only(EventList,ExamList),
            are_events_valid(EventList).

%is_valid(+S)
is_valid_check(schedule(EventList)) :-
            not(var(EventList)),
            exam_list(ExamsList),
            check_validity(EventList,ExamsList),
            !.

%is_valid(-S)
is_valid_check(schedule(EventList)) :-
            var(EventList),
            exam_list(ExamsList),
            create_schedule(schedule(EventList),ExamsList).