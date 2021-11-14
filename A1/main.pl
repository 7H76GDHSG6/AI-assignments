:- dynamic yes/1.

field_list(['coreCS', 'programming', 'ece', 'math', 'aI', 'design', 'ssh', 'biology', 'sociology', 'psychology', 'economics', 'misc']).

start:-
    undo,
    intro,
    stream(Tag1),
    recommend(L, Tag1),
    courseRecs(L).


intro:-
    write('Welcome to IIITD B.Tech elective advisory system, follow the program step by step to get your recommendations'), nl.


stream(Tag):-
    nl, write('Please enter your stream(enter common to get the common courses): '), nl,
    assert(yes(common)),
    read(Stream),
    (((Stream==common)-> (true));
    ((Stream==cse)-> (assert(yes(coreCS)), retract(yes(common))));
    ((Stream==csb)-> (assert(yes(biology)), retract(yes(common))));
    ((Stream==csd)-> (assert(yes(design)), retract(yes(common))));
    ((Stream==ece)-> (assert(yes(ece)), retract(yes(common))));
    ((Stream==csss)-> (assert(yes(ssh)), retract(yes(common))));
    ((Stream==csam)-> (assert(yes(math)), retract(yes(common))));
    ((Stream==csai)-> (assert(yes(aI)), retract(yes(common))))).


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
course(cse112, 'Computer Organisation', [ece111], [common, coreCS]).


/*
CS courses
*/
course(cse121, 'Discrete Mathematics', [], [coreCS]).
course(cse140, 'Introduction to Intelligent Systems', [], [aI]).
course(cse121, 'Advanced Programming', [cse101, cse102], [programming]).
course(cse202, 'Fundamentals of DBMS', [cse102], [coreCS]).
course(cse222, 'Agorithm Design and Analysis', [cse102], [programming, coreCS]).
course(cse231, 'Operating Systems', [cse102], [coreCS]).
course(cse232, 'Computer Networks', [cse101, cse231, cse222], [coreCS]).
course(cse342, 'Statistical Machine Learning', [cse101, mth201], [aI]).
course(cse343, 'Machine Learning', [mth100, mth201, cse101, mth203], [aI]).
course(cse344, 'Computer Vision', [mth100], [aI]).
course(cse345, 'Foundations of Computer Security', [], [coreCS]).
course(cse556, 'Natural Language Processing', [cse101, mth201, cse222, mth100], [aI]).
course(cse643, 'Artificial Intelligence', [cse102], [aI]).


/*
ECE courses
*/
course(ece113, 'Basic electronics', [], [ece]).
course(ece215, 'Circuit Theory and Devices', [], [ece]).
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
course(mth343, 'Introduction to Dynamical Systems', [mth100, mth203, mth240], [math]).


/*
Biology courses
*/
course(bio101, 'Foundations of Biology', [], [biology]).
course(bio211, 'Cell biology and Bio-Chemistry', [], [biology]).
course(bio101, 'Introduction to quantitative Biology', [mth100], [biology]).
course(bio321, 'Algorithms in Bioinformatics', [cse222], [biology]).
course(bio531, 'Introduction to Mathematical Biology', [mth100], [biology]).
course(bio533, 'Systems and Synthetic Biology', [bio531], [biology]).


/*
Design courses
*/
course(des101, 'Design Drawing and Visualisation', [], [design]).
course(des202, 'Visual design and communication', [des101], [design]).
course(des201, 'Design processes and perspectives', [des101], [design]).
course(des204, 'Human Computer Interaction', [], [design]).
course(des506, 'ATHCC', [des204], [design]).
course(des522, 'IDUDA', [des201], [design]).


/*
SSH courses
*/
course(eco201, 'Macroeconomics', [], [economics]).
course(eco221, 'Econometrics I', [mth201], [economics]).
course(eco223, 'Money and Banking', [], [economics]).
course(eco311, 'Game Theory', [], [economics]).
course(eco312, 'Industrial Organisation', [eco311], [economics]).
course(eco313, 'Market Design', [eco311], [economics]).
course(eco314, 'Behavioural economics', [mth201], [economics]).
course(eco322, 'Econometrics II', [eco221], [economics]).
course(eco341, 'Markov Decision Processes', [mth100, mth201, mth203], [economics]).

course(ssh101, 'CTRSS', [], [ssh]).
course(ssh201, 'RMSSD', [ssh101], [ssh]).
course(ssh215, 'Nation and her Narratives', [], [ssh]).

course(soc311, 'AERM', [ssh201], [sociology]).
course(soc101, 'Introduction to Sociology and Anthropology', [ssh101], [sociology]).
course(soc210, 'Sociological Theory', [], [sociology]).
course(soc302, 'Urban Sociology', [], [sociology]).

course(psy201, 'Introduction to Psychology', [ssh101], [psychology]).
course(psy202, 'Positive Psychology', [], [psychology]).
course(psy301, 'Cognitive Psychology', [], [psychology]).
course(psy302, 'Social Psychology', [], [psychology]).
course(psy308, 'Cognition of Motor Movement', [cse101, psy201], [psychology]).


/*
Misc Courses
*/
course(com301, 'Technical Communication', [], [misc]).
course(esc207, 'Ecology, Evolution, and Environment', [], [misc]).
course(ent411, 'Entrepreneurial Communication', [], [misc]).
course(ent413, 'Entrepreneurial Finance', [], [misc]).


undo :- retract(yes(_)), fail.
undo.