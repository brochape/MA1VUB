:- module(helpers,[ is_teacher/1, 
                    is_student/1, 
                    exam_list/1,
                    end/3,
                    gives_exam/2, 
                    takes_exam/2,
                    attends_exam/2,
                    max/3, 
                    min/3, 
                    has_event_with_exam/3,
                    during_session/1, 
                    simple_event_validation/1, 
                    can_students_fit/2,
                    number_of_students/2,
                    is_room_available/1

                    ]).

is_teacher(PID):- lecturer(PID,_).
is_student(PID):- student(PID,_).


%event(EID,RID,Day,Start)
end(Exam, Start , End) :- 
            duration(Exam,Duration),
            End is Start+Duration.

takes_exam(Student, Exam) :- follows(Student, Course), has_exam(Course,Exam).
gives_exam(Teacher, Exam) :- teaches(Teacher, Course), has_exam(Course, Exam).

attends_exam(Person, Exam) :- takes_exam(Person, Exam).
attends_exam(Person, Exam) :- gives_exam(Person, Exam).

%max(+M, +N, -O)
max(M,N,O):-
            O is max(M,N).

%min(+M, +N, -O)
min(M,N,O):-
            O is min(M,N).

%has_event_with_exam(+EventList, +EID, ?Event)
has_event_with_exam(EventList, EID, event(EID, RID, Day, Start)):-
            member(event(EID, RID, Day, Start), EventList),
            !.

%exam_list(-List)
exam_list(List):- 
            findall(ExamID, exam(ExamID,_), List).

during_session(Day) :- 
            first_day(Start),
            last_day(End),
            between(Start,End,Day).


number_of_students(Exam, StudNum) :- findall(Student,takes_exam(Student,Exam),Lst), length(Lst, StudNum), !.

can_students_fit(Exam,Room) :- number_of_students(Exam, StudNum), capacity(Room,Capacity), StudNum=<Capacity .


is_room_available( event(Exam,Room,Day,Start) ) :-
            availability(Room, Day, X, Y), 
            between(X,Y,Start),
            end(Exam, Start, End), 
            Y >= End.

simple_event_validation(event(EID,RID,Day,Start)):-
            exam(EID,_),
            room(RID,_),
            during_session(Day),
            can_students_fit(EID,RID),
            is_room_available(event(EID,RID,Day,Start)).