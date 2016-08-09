
are_all_exams_planned_once(schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)]))
are_all_exams_planned_once(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]))
pretty_print(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
consult('small_instance.pl').

write('=== Valid schedules ==='),
is_valid(schedule([event(e2, r2, 2, 10), event(e1, r2, 1, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])).
is_valid(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
is_valid(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 13)])).
is_valid(schedule([event(e1, r2, 3, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)])).
is_valid(schedule([event(e1, r2, 3, 13), event(e5, r2, 2, 10), event(e2, r2, 1, 10), event(e4, r1, 3, 11), event(e3, r2, 3, 11)])).

write('Not all exams included in session (1)'),
not(is_valid(schedule([event(e1, r2, 1, 10)]))).

write('Not all exams included in session (2)'),
not(is_valid(schedule([event(e3, r1, 1, 10), event(e4, r2, 2, 10)]))).

write('exam e5 is scheduled twice'),
not(is_valid(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10), event(e5, r2, 5, 10)]))).

write('exam e3 does not start on a the hour'),
not(is_valid(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10.5), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]))).

write('r2 is not available the whole time during e1'),
not(is_valid(schedule([event(e1, r2, 1, 11), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]))).

write('r1 is not large enough for all students taking e1'),
not(is_valid(schedule([event(e1, r1, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]))).

write('e3 and e4 are held concurrently in the same room'),
not(is_valid(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 10), event(e4, r2, 3, 11), event(e5, r2, 5, 10)]))).

write('e1 and e3 are held concurrently while 2 students take both exams'),
not(is_valid(schedule([event(e1, r2, 3, 11), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r2, 4, 10), event(e5, r2, 5, 10)]))).

write('room 1 is too small'),
not(is_valid(schedule([event(e1, r1, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 5, 10), event(e4, r1, 4, 10), event(e5, r2, 3, 12)]))).