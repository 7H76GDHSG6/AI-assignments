:- dynamic yes/1, no/1.
:- dynamic stream/1.

field_list([soft_dev, finance, sciences, design, hardware]).

start:-
    undo,
    intro,
    stream,
    getCourses(L),
    courseRecs(L).


intro:-
    write('Welcome to IIITD B.Tech elective advisory system, follow the program step by step to get your recommendations'), nl.


stream:-
    nl, write('Please enter your stream: '), nl,
    read(Stream),
    assert(yes(Stream)).


getCourses(L):-
    nl, write('Enter the courses you have taken till now(Ex - [cse101, com101]): '), nl,
    read(CoursesTaken),
    nl, write('Which fields are you interested in? (y or n): '), nl,
    field_list(Fields),
    fieldCheck(Fields),

    /*
    Suggest electives to be taken
    */
    advise(CoursesTaken, L).

fieldCheck([]).
fieldCheck([H|T]) :- process(H), fieldCheck(T).

process(A):-
    format('Are you interested in - ~k:', [A]),
    read(V),
    ((V==y)->   assert(yes(A));
    assert(no(A))).

/*
Finding recommendations
*/
advise(CoursesTaken, L):-
    nl, write('The recommended course for you are'), nl, nl,
    findall(X, check(X, CoursesTaken), L).

check(X, CoursesTaken):-
    course(_, X, _, _).

/*
Printing the recommended courses
*/
courseRecs([]).
courseRecs([H|T]):-
    write(H), nl, courseRecs(T).


/*
Elective Courses list
course(C, N, P, T) is true for course C with name N, prerequisites P, and tags T
*/

/*
Intro courses (first 2 sems)
*/
course(cse101, 'Introduction to Programming', [], ['common', 'programming']).
course(cse102, 'Data Structures & Algorithms', [cse101], ['common', 'programming']).


undo :- retract(yes(_)), fail. 
undo :- retract(no(_)), fail.
undo.