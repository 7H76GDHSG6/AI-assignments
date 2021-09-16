:- dynamic yes/1, no/1.
:- dynamic stream/1.

field_list([soft_dev, finance, sciences, design, hardware]).

start:-
    intro,
    getCourses,
    undo.

intro:-
    write('Welcome to IIITD B.Tech elective advisory system, follow the program step by step to get your recommendations'), nl.

getCourses:-
    nl, write('Please enter your stream: '), nl,
    read(Stream),
    nl, write('Enter the courses you have taken till now(Ex - [cse101, com101]): '), nl,
    read(CoursesTaken),
    nl, write('Which fields are you interested in? (y or n): '), nl,
    field_list(Fields),
    fieldCheck(Fields),

    /*
    Suggest electives to be taken
    */
    advise(Stream, CoursesTaken).

fieldCheck([]).
fieldCheck([H|T]) :- process(H), fieldCheck(T).

process(A):-
    format('Are you interested in - ~k:', [A]),
    read(V),
    ((V==y)->   assert(yes(A));
    assert(no(A))).

advise(S, L):-
    write('test2').

/*
Elective Courses list
*/


undo :- retract(yes(_)), fail. 
undo :- retract(no(_)), fail.
undo :- retract(course(_, _, _, _, _)), fail.
undo.