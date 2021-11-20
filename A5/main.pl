:- dynamic yes/1.
:- dynamic taglist/1.
:- dynamic streamname/1.

field_list(['compsci', 'programming', 'ece', 'math', 'ai', 'design', 'ssh', 'biology', 'sociology', 'psychology', 'economics', 'misc']).

start:-
    undo,
    intro,
    streamhelp(Tag1),
    recommend(L, Tag1),
    courseRecs(L).


intro:-
    write('Welcome to IIITD B.Tech elective advisory system, follow the program step by step to get your recommendations'), nl.

streamhelp(Tag1):-
    nl, write('Please enter your stream(enter common to get the common courses): '), nl,
    assert(yes(common)),
    read(Stream),
    stream(Stream, Tag1).

stream(Stream, Tag):-
    (((Stream==common)-> (true));
    ((Stream==cse)-> (assert(yes(compsci)), Tag=compsci, retract(yes(common))));
    ((Stream==csb)-> (assert(yes(biology)), Tag=biology, retract(yes(common))));
    ((Stream==csd)-> (assert(yes(design)), Tag=design, retract(yes(common))));
    ((Stream==ece)-> (assert(yes(ece)), Tag=ece, retract(yes(common))));
    ((Stream==csss)-> (assert(yes(ssh)), Tag=ssh, retract(yes(common))));
    ((Stream==csam)-> (assert(yes(math)), Tag=math, retract(yes(common))));
    ((Stream==csai)-> (assert(yes(ai)), Tag=ai, retract(yes(common))))).


recommend(L, Tag1):-
    nl, write('Enter the courses you have taken till now(Enter end to stop): '), nl,
    getCourses(CoursesTaken),
    nl, write('Enter the type of course you are interested in?: '), nl,
    read(Tag2),
    /*
    Suggest electives to be taken
    */
    advise(CoursesTaken, L, [Tag1, Tag2]).

getCourses(X):-  read(A), stop(A, X-[]).
stop(end, X-X):- !.
stop(A, [A|X]-Y):- read(B), stop(B, X-Y).

/*
Finding recommendations
*/
advise(CoursesTaken, L, Tags):-
    nl, write('The recommended course for you are'), nl, nl,
    setof(X, X^check(X, CoursesTaken, Tags), L).

check(X, CoursesTaken, Tags):-
    % Code for checking predicates is above the courses
    check_pre_req(CoursesTaken, X),
    check_tags(Tags, X).

/*
Printing the recommended courses
*/
courseRecs([]).
courseRecs([H|T]):-
    write(H), nl, courseRecs(T).


/*
Checking predicates
*/
check_pre_req(CoursesTaken, X):-
    course(_, X, Pre, _),
    checkMembers(CoursesTaken, Pre).

checkMembers(_,[]).
checkMembers(L,[H|T]) :-
	member(H,L),
	checkMembers(L,T).

check_tags(Tags, X):-
    course(_, X, _, TagsC),
    applicableTag(Tags, TagsC).

applicableTag([H|T], TagsC):-
    applicableTag(T, TagsC).
applicableTag([H|T], TagsC):-
    member(H, TagsC).

/*
With Python NLP
*/
connectP:-
    retractall(taglist(_)),
    retractall(streamname(_)),
    consult("f:/some_git_repos/AI-assignments/A5/facts.pl"),
    taglist(Tags),
    % streamname(Stream),
    % streamP(Tags, Stream, LT),
    adviseP(Courses, Tags),
    courseRecs(Courses).

streamP(Tags, Stream, L):-
    (((Stream==common)-> (append(Tags, [common], L)));
    ((Stream==cse)-> (assert(yes(compsci)), append(Tags, [compsci], L), retract(yes(common))));
    ((Stream==csb)-> (assert(yes(biology)), append(Tags, [biology], L), retract(yes(common))));
    ((Stream==csd)-> (assert(yes(design)), append(Tags, [design], L), retract(yes(common))));
    ((Stream==ece)-> (assert(yes(ece)), append(Tags, [ece], L), retract(yes(common))));
    ((Stream==csss)-> (assert(yes(ssh)), append(Tags, [ssh], L), retract(yes(common))));
    ((Stream==csam)-> (assert(yes(math)), append(Tags, [math], L), retract(yes(common))));
    ((Stream==csai)-> (assert(yes(ai)), append(Tags, [ai], L), retract(yes(common))))).

adviseP(L, Tags):-
    nl, write('The recommended course for you are'), nl, nl,
    setof(X, X^checkP(X, Tags), L).

checkP(X, Tags):-
    check_tags(Tags, X).

/*
Elective Courses list
course(C, N, P, T) is true for course C with name N, prerequisites P, and tags T

Intro courses (first 2 sems)
*/
course(cse101, 'Introduction to Programming', [], [common, programming]).
course(ece111, 'Digital Circuits', [], [common, ece]).
course(mth100, 'Math I- Linear Algebra', [], [common, math]).
course(des102, 'Introduction to HCI', [], [common, design]).
course(com101, 'Communication Skills', [], [common, misc]).
course(cse102, 'Data Structures & Algorithms', [cse101], [common, programming]).
course(mth201, 'Probability & Statistics', [], [common, math]).
course(cse112, 'Computer Organisation', [ece111], [common, compsci]).


/*
CS courses
*/
course(cse140, 'Introduction to Intelligent Systems', [], [ai]).
course(cse121, 'Advanced Programming', [cse101, cse102], [programming]).
course(cse202, 'Fundamentals of DBMS', [cse102], [compsci]).
course(cse222, 'Agorithm Design and Analysis', [cse102], [programming, compsci]).
course(cse231, 'Operating Systems', [cse102], [compsci]).
course(cse232, 'Computer Networks', [cse101, cse231, cse222], [compsci]).
course(cse342, 'Statistical Machine Learning', [cse101, mth201], [ai]).
course(cse343, 'Machine Learning', [mth100, mth201, cse101, mth203], [ai]).
course(cse344, 'Computer Vision', [mth100], [ai]).
course(cse345, 'Foundations of Computer Security', [], [compsci]).
course(cse556, 'Natural Language Processing', [cse101, mth201, cse222, mth100], [ai]).
course(cse643, 'Artificial Intelligence', [cse102], [ai]).


/*
ECE courses
*/
course(ece113, 'Basic electronics', [], [ece]).
course(ece214, 'Integrated electronics', [ece111, ece113], [ece]).
course(ece250, 'Signals and Systems', [mth100], [ece]).
course(ece240, 'Principles of Communication Systems', [ece250, mth201], [ece]).
course(ece340, 'Digital Image Processing', [mth100, mth201], [ece]).


/*
Math courses
*/
course(mth203, 'Multivariate Calculus', [], [math]).
course(mth204, 'Differential Equations', [mth203], [math]).
course(mth240, 'Real Analysis I', [], [math]).
course(mth340, 'Real Analysis II', [mth240], [math]).
course(mth270, 'Numerical Methods', [mth100, mth204], [math]).


/*
Biology courses
*/
course(bio101, 'Foundations of Biology', [], [biology]).
course(bio211, 'Cell biology and Bio-Chemistry', [], [biology]).
course(bio101, 'Introduction to quantitative Biology', [mth100], [biology]).
course(bio321, 'Algorithms in Bioinformatics', [cse222], [biology]).


/*
Design courses
*/
course(des101, 'Design Drawing and Visualisation', [], [design]).
course(des202, 'Visual design and communication', [des101], [design]).
course(des201, 'Design processes and perspectives', [des101], [design]).
course(des204, 'Human Computer Interaction', [], [design]).


/*
SSH courses
*/
course(eco201, 'Macroeconomics', [], [economics]).
course(eco221, 'Econometrics I', [mth201], [economics]).
course(eco223, 'Money and Banking', [], [economics]).
course(eco311, 'Game Theory', [], [economics]).
course(eco322, 'Econometrics II', [eco221], [economics]).

course(ssh101, 'CTRSS', [], [ssh]).
course(ssh201, 'RMSSD', [ssh101], [ssh]).
course(ssh215, 'Nation and her Narratives', [], [ssh]).

course(soc101, 'Introduction to Sociology and Anthropology', [ssh101], [sociology]).
course(soc210, 'Sociological Theory', [], [sociology]).
course(soc302, 'Urban Sociology', [], [sociology]).

course(psy201, 'Introduction to Psychology', [ssh101], [psychology]).
course(psy202, 'Positive Psychology', [], [psychology]).
course(psy301, 'Cognitive Psychology', [], [psychology]).
course(psy302, 'Social Psychology', [], [psychology]).


/*
Misc Courses
*/
course(com301, 'Technical Communication', [], [misc]).
course(esc207, 'Ecology, Evolution, and Environment', [], [misc]).
course(ent411, 'Entrepreneurial Communication', [], [misc]).
course(ent413, 'Entrepreneurial Finance', [], [misc]).


undo :- retract(yes(_)), fail.
undo.