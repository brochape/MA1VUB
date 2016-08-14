% This files contains some examples which you can use to validate your solution.
% All examples below are for the small instance.
% Disclaimer: Correct behavior for these examples, does not guarantee your solution is in fact correct. 



%Some examples of invalid schedules

/*
schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10)]) %missing exam e5
schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10), event(e5, r2, 5, 10)]) %exam e5 is scheduled twice
schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10.5), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]) %exam e3 doesn't start on a the hour
schedule([event(e1, r2, 1, 11), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]) %r2 isn't available the whole time during e1
schedule([event(e1, r1, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]) %r1 isn't large enough for all students taking e1
schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 3, 11), event(e5, r2, 5, 10)]) %e3 and e4 are held concurrently in the same room
schedule([event(e1, r2, 3, 11), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]) %e1 and e3 are held concurrently while 2 students take both exams
*/

:- discontiguous 
    is_valid/1,
    cost/2,
    violated_sc/2.

%Examples of valid schedules, their cost and soft-constraints they violate

% schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])
%l2 has 1 day too little to correct and all students have 2 days too little to study.
is_valid(schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])).
cost(schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)]),3.375).
violates_sc(schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)]),[sc_correction_time(l2, 1, 3), sc_study_time(s1, 2, 6), sc_study_time(s2, 2, 6), sc_study_time(s3, 2, 6), sc_study_time(s4, 2, 6)]).

% schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])
% similar to the first example, but e5 is now held during the lunch break
is_valid(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
cost(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]),4).
violates_sc(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]),[sc_correction_time(l2, 1, 3), sc_lunch_break(l4, e5, 1), sc_study_time(s1, 2, 6), sc_lunch_break(s1, e5, 1), sc_study_time(s2, 2, 6), sc_lunch_break(s2, e5, 1), sc_study_time(s3, 2, 6), sc_lunch_break(s3, e5, 1), sc_study_time(s4, 2, 6), sc_lunch_break(s4, e5, 1)]).

% schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])
% similar to the first example, but e1 is now held the same day as e5
is_valid(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])).
cost(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)]),4.375).
violates_sc(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)]),[sc_correction_time(l2, 1, 3), sc_same_day(s1, e1, e5, 2), sc_study_time(s1, 2, 6), sc_same_day(s2, e1, e5, 2), sc_study_time(s2, 2, 6), sc_same_day(s3, e1, e5, 2), sc_study_time(s3, 2, 6), sc_same_day(s4, e1, e5, 2), sc_study_time(s4, 2, 6)]).

% schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])
% similar to previous example, but e5 now starts right after e1 (and is therefore also held during lunch break)
is_valid(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
cost(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]),7.5).
violates_sc(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]),[sc_correction_time(l2, 1, 3), sc_lunch_break(l4, e5, 1), sc_same_day(s1, e1, e5, 2), sc_b2b(s1, e1, e5, 5), sc_study_time(s1, 2, 6), sc_lunch_break(s1, e5, 1), sc_same_day(s2, e1, e5, 2), sc_b2b(s2, e1, e5, 5), sc_study_time(s2, 2, 6), sc_lunch_break(s2, e5, 1), sc_same_day(s3, e1, e5, 2), sc_b2b(s3, e1, e5, 5), sc_study_time(s3, 2, 6), sc_lunch_break(s3, e5, 1), sc_same_day(s4, e1, e5, 2), sc_b2b(s4, e1, e5, 5), sc_study_time(s4, 2, 6), sc_lunch_break(s4, e5, 1)]).

% schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)])
% this is one of the worst possible valid exam schedules...
is_valid(schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)])).
cost(schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)]),10.5).
violates_sc(schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)]),[sc_not_in_period(l1, e2, 1, 0, 24, 3), sc_no_exam_in_period(l1, e1, 3, 14, 24, 5), sc_lunch_break(l2, e3, 1), sc_lunch_break(l3, e4, 1), sc_no_exam_in_period(l3, e4, 3, 0, 24, 5), sc_no_exam_in_period(l4, e5, 2, 0, 12, 1), sc_same_day(s1, e4, e1, 2), sc_b2b(s1, e4, e1, 5), sc_study_time(s1, 3, 9), sc_lunch_break(s1, e4, 1), sc_same_day(s2, e3, e1, 2), sc_b2b(s2, e3, e1, 5), sc_study_time(s2, 3, 9), sc_lunch_break(s2, e3, 1), sc_same_day(s3, e3, e1, 2), sc_b2b(s3, e3, e1, 5), sc_study_time(s3, 3, 9), sc_lunch_break(s3, e3, 1), sc_same_day(s4, e4, e1, 2), sc_b2b(s4, e4, e1, 5), sc_study_time(s4, 3, 9), sc_lunch_break(s4, e4, 1)]).



%% violates_sc(schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)]),X).

%% permutation(
%% [sc_not_in_period(l1, e2, 1, 0, 24, 3), sc_no_exam_in_period(l1, e1, 3, 14, 24, 5), sc_lunch_break(l2, e3, 1), sc_lunch_break(l3, e4, 1), sc_no_exam_in_period(l3, e4, 3, 0, 24, 5), sc_no_exam_in_period(l4, e5, 2, 0, 12, 1), sc_lunch_break(s3, e3, 1), sc_lunch_break(s2, e3, 1), sc_lunch_break(s4, e4, 1), sc_lunch_break(s1, e4, 1), sc_study_time(s1, 3, 9), sc_study_time(s2, 3, 9), sc_study_time(s3, 3, 9), sc_study_time(s4, 3, 9), sc_b2b(s2, e1, e3, 5), sc_b2b(s3, e1, e3, 5), sc_b2b(s1, e1, e4, 5), sc_b2b(s4, e1, e4, 5), sc_same_day(s2, e1, e3, 2), sc_same_day(s3, e1, e3, 2), sc_same_day(s1, e1, e4, 2), sc_same_day(s4, e1, e4, 2)],
%% [sc_not_in_period(l1, e2, 1, 0, 24, 3), sc_no_exam_in_period(l1, e1, 3, 14, 24, 5), sc_lunch_break(l2, e3, 1), sc_lunch_break(l3, e4, 1), sc_no_exam_in_period(l3, e4, 3, 0, 24, 5), sc_no_exam_in_period(l4, e5, 2, 0, 12, 1), sc_same_day(s1, e4, e1, 2), sc_b2b(s1, e4, e1, 5), sc_study_time(s1, 3, 9), sc_lunch_break(s1, e4, 1), sc_same_day(s2, e3, e1, 2), sc_b2b(s2, e3, e1, 5), sc_study_time(s2, 3, 9), sc_lunch_break(s2, e3, 1), sc_same_day(s3, e3, e1, 2), sc_b2b(s3, e3, e1, 5), sc_study_time(s3, 3, 9), sc_lunch_break(s3, e3, 1), sc_same_day(s4, e4, e1, 2), sc_b2b(s4, e4, e1, 5), sc_study_time(s4, 3, 9), sc_lunch_break(s4, e4, 1)]).

violates_sc(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]),X).
is_optimal(schedule([event(e1, r2, 2, 10), event(e2, r2, 5, 10), event(e3, r1, 4, 10), event(e4, r2, 4, 10), event(e5, r2, 3, 13)]).