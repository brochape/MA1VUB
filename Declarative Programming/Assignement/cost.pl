:- module(cost,[cost/2, violates_sc/2]).

:- use_module(helpers,[end/3,gives_exam/2, takes_exam/2,attends_exam/2, is_teacher/1, is_student/1, max/3, min/3, has_event_with_exam/3]).
:- use_module(sort,[sort_schedule/2]).



%%===================S_C======================
%%       CHECKS ALL THE SOFT CONSTRAINTS
%%============================================

%overlaps_period(+Start, +End, +PeriodStart, +PeriodEnd)
overlaps_period(Start, End, PeriodStart, PeriodEnd) :-
            ((Start >= PeriodStart, Start < PeriodEnd) ;
            (End > PeriodStart, End =< PeriodEnd)).

violates_lunch_break(event(EID,_,_,Start),EID,PID,Penalty):-
            attends_exam(PID, EID),
            end(EID, Start, End),
            sc_lunch_break(PID, Penalty),
            overlaps_period(Start, End, 12, 13).

%No exam in period for teachers
violates_no_exam_in_period(event(EID, _, Day, Start), EID, LID, From, Till, Penalty):-
            gives_exam(LID, EID),
            end(EID, Start, End),
            sc_no_exam_in_period(LID, Day, From, Till, Penalty),
            overlaps_period(Start, End, From, Till).

violates_not_in_period(event(EID, _, Day, Start), EID, PID, From, Till, Penalty):-
            attends_exam(PID, EID),
            end(EID, Start, End),
            sc_not_in_period(PID, EID, Day, From, Till, Penalty),
            overlaps_period(Start, End, From, Till).

%Complex violation, 2+ exams concerned

      


violates_same_day(EventList, sc_same_day(PID,EID, EID2, Penalty)):-
            exam(EID,_),
            exam(EID2,_),
            EID @< EID2, %Needs a way to not insert permutations
            has_event_with_exam(EventList, EID, event(EID, _, Day, Start)),
            has_event_with_exam(EventList, EID2, event(EID2, _, Day, Start2)),
            attends_exam(PID, EID),
            attends_exam(PID, EID2),
            sc_same_day(PID, Penalty).


violates_b2b(EventList, sc_b2b(PID, EID, EID2, Penalty)) :-
            
            exam(EID,_),
            exam(EID2,_),
            %% EID @< EID2, %Needs a way to not insert permutations
            has_event_with_exam(EventList, EID, event(EID, _, Day, Start)),
            has_event_with_exam(EventList, EID2, event(EID2, _, Day, Start2)),
            attends_exam(PID, EID),
            attends_exam(PID, EID2),

            end(EID, Start, End),
            end(EID2, Start2, End2),
            End2 is Start,
            sc_b2b(PID, Penalty).


%% violates_b2b_loop(Event, [Event2|Events], ):-%   

violates_correction_time(EventList, sc_correction_time(PID, DTL, TotalPenalty)):-
            bagof((Day, TimeToCorrect),
                      EventList^EID^RID^Start^(
                        gives_exam(PID, EID),
                        has_event_with_exam(EventList, EID, event(EID, RID, Day, Start)),
                        sc_correction_time(EID, TimeToCorrect)),
                      DTs
                            ),
            sort(1,@>=,DTs, SortedByDay),
            last_day(LastDay),
            calculate_DTL_correction(SortedByDay, LastDay, 0, DTL),
            DTL > 0,%If neg or =0, the teacher has enough time -> no penalty
            sc_correction_penalty(PID, Penalty),
            TotalPenalty is DTL*Penalty.

violates_study_time(EventList, sc_study_time(PID, DTL, TotalPenalty)):-
            first_day(FirstDay),
            bagof((Day, TimeToStudy),
                            EventList^EID^RID^Start^(
                              takes_exam(PID, EID),
                              has_event_with_exam(EventList, EID, event(EID, RID, Day, Start)),
                              sc_study_time(EID, TimeToStudy)),
                      DTs
                            ),

            sort(1,@=<,DTs, SortedByDay),
            calculate_DTL_study(SortedByDay, FirstDay, 0, DTL),
            DTL > 0,%If neg or =0, the teacher has enough time -> no penalty
            sc_study_penalty(PID, Penalty),
            TotalPenalty is DTL*Penalty.



calculate_DTL_correction([],_,DTL,DTL).
calculate_DTL_correction([(Day,TimeToCorrect)|DTs],CurrentLastDay,DTL,Total):-
            AvailableTime is CurrentLastDay - Day,
            Interval is TimeToCorrect - AvailableTime,
            max(0,Interval,Shortage),%Shortage is the amount of days extra that would be needed
            NewDTL is DTL + Shortage,
            NextDay is CurrentLastDay - TimeToCorrect,
            max(Day,NextDay,ActualNextDay),
            calculate_DTL_correction(DTs,ActualNextDay,NewDTL,Total).



calculate_DTL_study([],_,DTL,DTL).
calculate_DTL_study([(Day,TimeToStudy)|DTs],CurrentFirstDay,DTL,Total):-
            AvailableTime is Day - CurrentFirstDay,
            Interval is TimeToStudy - AvailableTime,
            max(0,Interval,Shortage),%Shortage is the amount of days extra that would be needed
            NewDTL is DTL + Shortage,
            NextDay is CurrentFirstDay + TimeToStudy,
            min(Day,NextDay,NewNextDay),
            calculate_DTL_study(DTs,NewNextDay,NewDTL,Total).


violates_sc_correction_time(EventList, ViolatedConstraintsList):-
            findall(X, violates_correction_time(EventList,X), ViolatedConstraintsList),
            !.

violates_sc_study_time(EventList, ViolatedConstraintsList):-
            findall(X, violates_study_time(EventList,X), ViolatedConstraintsList),
            !.


violates_sc_b2b(EventList, ViolatedConstraintsList):-
            findall(X,violates_b2b(EventList,X),ViolatedConstraintsList),
            !.

violates_sc_same_day(EventList, ViolatedConstraintsList) :-
            findall(X,violates_same_day(EventList,X),ViolatedConstraintsList),
            !.   

violated_constraints_simple([],ViolatedConstraintsList,ViolatedConstraintsList).

violated_constraints_simple([Event|Events],ViolatedConstraintsList,Constr):-
            violates_lunch_break(Event,EID,PID,Penalty),
            not(member(sc_lunch_break(PID, EID, Penalty),ViolatedConstraintsList)),%Violated constraint not taken into account yet
            violated_constraints_simple([Event|Events], [sc_lunch_break(PID, EID, Penalty)|ViolatedConstraintsList],Constr).

violated_constraints_simple([Event|Events],ViolatedConstraintsList,Constr):-
            violates_no_exam_in_period(Event, EID, PID, From, Till, Penalty),
            is_on_day(Event, Day),
            not(member(sc_no_exam_in_period(PID, EID, Day, From, Till, Penalty),ViolatedConstraintsList)),%Violated constraint not taken into account yet
            violated_constraints_simple([Event|Events], [sc_no_exam_in_period(PID, EID, Day, From, Till, Penalty)|ViolatedConstraintsList],Constr).

violated_constraints_simple([Event|Events],ViolatedConstraintsList,Constr):-
            violates_not_in_period(Event, EID, PID, From, Till, Penalty),
            is_on_day(Event, Day),
            not(member(sc_not_in_period(PID, EID, Day, From, Till, Penalty),ViolatedConstraintsList)),%Violated constraint not taken into account yet
            violated_constraints_simple([Event|Events], [sc_not_in_period(PID, EID, Day, From, Till, Penalty)|ViolatedConstraintsList],Constr).

%If reaches here, it means the event doesn't break any constraint
violated_constraints_simple([_|Events], ViolatedConstraintsList, Constr):-
            violated_constraints_simple(Events,ViolatedConstraintsList, Constr).
            


violated_constraints_complex(EventList, ViolatedConstraintsList):-
            sort_schedule(schedule(EventList), SortedSchedule),
            violates_sc_correction_time(SortedSchedule, CorrectionConstraintsList),
            violates_sc_study_time(SortedSchedule, StudyConstraintsList),
            violates_sc_b2b(SortedSchedule,B2bConstraintsList),
            violates_sc_same_day(EventList, SameDayConstraintsList),
            append([CorrectionConstraintsList, StudyConstraintsList, B2bConstraintsList, SameDayConstraintsList], ViolatedConstraintsList),

            !.


apply_violation(PreviousPenalty, PenaltyToAdd, NewPenalty):-
            NewPenalty is PreviousPenalty+PenaltyToAdd.


penalties_count(Schedule, TeacherPen, StudentsPen):-
            violates_sc(Schedule, ViolatedConstraintsList),%here, ViolatedConstraintsList is empty
            penalties_count_inner(ViolatedConstraintsList, TeacherPen, 0, StudentsPen, 0).



penalties_count_inner([], TeacherPen,TeacherPen, StudentsPen,StudentsPen):-!.
penalties_count_inner([ViolatedConstraint|ConstraintsList], TeacherPen, TeacherPenAccum, StudentsPen, StudentsPenAccum):-
            is_teacher(PID),
            penalty_pid_for_constraint(ViolatedConstraint,PID, Penalty),
            NewTeacherPenalty is TeacherPenAccum + Penalty,
            penalties_count_inner(ConstraintsList, TeacherPen, NewTeacherPenalty, StudentsPen, StudentsPenAccum).

penalties_count_inner([ViolatedConstraint|ConstraintsList], TeacherPen, TeacherPenAccum, StudentsPen, StudentsPenAccum):-
            penalty_pid_for_constraint(ViolatedConstraint,PID, Penalty),
            is_student(PID),
            NewStudentPenalty is StudentsPenAccum + Penalty,
            penalties_count_inner(ConstraintsList, TeacherPen, TeacherPenAccum, StudentsPen, NewStudentPenalty).

% Generates the list of violated constraints
violates_sc(schedule(EventsList), ViolatedConstraints):-
            violated_constraints_simple(EventsList, [],SimpleViolatedConstraintsList),
            violated_constraints_complex(EventsList, ComplexViolatedConstraintsList),
            append(SimpleViolatedConstraintsList,ComplexViolatedConstraintsList,ViolatedConstraintsList),
            permutation(ViolatedConstraintsList,ViolatedConstraints),
            !.% Only need one



is_on_day(event(_,_,Day,_), Day).

penalty_pid_for_constraint(sc_not_in_period(PID,_,_,_,_,Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_no_exam_in_period(PID,_,_,_,_,Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_lunch_break(PID,_,Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_same_day(PID,_,_,Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_b2b(PID,_,_,Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_correction_time(PID, _, Pen), PID, Pen):-!.
penalty_pid_for_constraint(sc_study_time(PID, _, Pen), PID, Pen):-!.




total_amount_students(Amount) :-
            findall(Student,student(Student,_), Studs),
            length(Studs,Amount),
            !.
total_amount_teachers(Amount) :-
            findall(Teacher,lecturer(Teacher,_), TeachersList),
            length(TeachersList,Amount),
            !.

cost(Schedule, Cost) :-
            %% write('hi'),
            penalties_count(Schedule, TeachersPenalty, StudentsPenalty),
            total_amount_teachers(TeachersAmount),
            total_amount_students(StudentsAmount),
            Cost is ((TeachersPenalty/TeachersAmount + StudentsPenalty/StudentsAmount) / 2),
            !.
