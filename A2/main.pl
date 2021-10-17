:- [library(csv)].

:- dynamic distance/3.
:- dynamic key/1.
:- dynamic h/2.
:- dynamic city/1.

/*
Reading from csv and aserting predicates
*/

filename('roaddistance.csv').

readfile:-
    undo,
    filename(File),
    forall(readrow(File, Row), storerow(Row)).

readrow(File, Row):-
    csv_read_file_row(File, Row, [strip(true), convert(true)]),
    writeln(readrow(Row)).

storerow(Row):-
    Row =.. [row|Cols],
    Cities = [],
    (   key(ColKeys) ->
            Cols = [RowKey|Samples],
            assert(distance(RowKey, RowKey, 0)),
            assert(city(RowKey)),
            maplist(storesample(RowKey), ColKeys, Samples);
        assert(key(Cols))
    ).

storesample(RowKey, ColKey, Value):-
    assert(distance(RowKey, ColKey, Value)),
    assert(distance(ColKey, RowKey, Value)).

/*
Depth First Search
*/

dfs(Start, End , Path, InitCost, FinalCost):-
    Start == End ->
        writeln('Same start and end point'),
        Path = [],
        FinalCost = 0;
    path(Start, End, [Start], L, InitCost, FinalCost),
    reverse(L,Path).

path(Cur, Next, Path, [Next|Path], Cost, FinalCost):-
   distance(Cur, Next, Dist),
   number(Dist),
   FinalCost is Cost + Dist.

path(Cur, Next, L, Path, Cost, FinalCost):-
   distance(Cur, Any, Dist), 
   \+ member(Any, L),          % avoid visited nodes
   number(Dist),
   NewCost is Cost + Dist,
   path(Any, Next, [Any|L], Path, NewCost, FinalCost).

/*
Best First Search - Helper functions
*/

bestfs(Start, End, Path, InitCost, FinalCost):-
    Start == End ->
        writeln('Same start and end point'),
        Path = [],
        FinalCost = 0;
    findall(X, city(X), L),
    delH,
    build_heuristic(L, End),
    best_first([[Start]], End, Temp, NumNodes),
    % bfs(Start, End, Path, Init, Final),
    reverse(Temp, Path),
    findCost(Path, 0, FinalCost).

build_heuristic([], End).
build_heuristic([H|T], End):-
    findH(H, End),
    build_heuristic(T, End).

findH(City, End):-
    findall(X, pathcost(City, End, X), L),
    min_list(L, Min),
    \+ h(City, Min) -> assert(h(City, Min));
    true.

pathcost(City, End, Cost):-
    distance(City, Any, D1),
    number(D1),
    distance(Any, End, D2),
    number(D2),
    Cost is D1 + D2.

findCost([A|[B|[]]], CurCost, FinalCost):-
    distance(A, B, Dist),
    FinalCost is CurCost + Dist.

findCost([A|[B|C]], CurCost, FinalCost):-
    distance(A, B, Dist),
    NewCost is CurCost + Dist,
    findCost([B|C], NewCost, FinalCost).

/*
Best first Search
*/

best_first([[Goal|Path]|_], Goal, [Goal|Path], 0).
best_first([Path|Queue], Goal, FinalPath, N):-
    extend(Path, NewPaths), 
    append(Queue, NewPaths, Queue1),
    sort_queue1(Queue1, NewQueue),
    best_first(NewQueue, Goal, FinalPath, M),
    N is M+1.

extend([Node|Path], NewPaths):-
    findall([NewNode, Node|Path],
            (distance(Node, NewNode, _), 
            \+ member(NewNode, Path)),
            NewPaths).

sort_queue1(L, L2):-
    swap1(L, L1), !,
    sort_queue1(L1, L2).
sort_queue1(L, L).

swap1([[A1|B1], [A2|B2]|T], [[A2|B2], [A1|B1]|T]):-
    h(A1, W1),
    h(A2, W2),
    W1>W2.
swap1([X|T], [X|V]):-
    swap1(T, V).

/*
Running the program
*/

start:-
    readfile,
    writeln('\nChoose the search method'),
    writeln('1. Depth First Search'),
    writeln('2. Best First Search'),
    read(X),
    (
        X==1 ->
            write('Enter Start city: '),
            read(Start),
            write('Enter End city: '),
            read(End),
            writeln(''),
            dfs(Start, End, Path, 0, Cost),
            (
                nonvar(Cost) ->
                    write('Path: '),
                    writeln(Path),
                    write('Cost: '),
                    writeln(Cost)
            );
        X==2 ->
            write('Enter Start city: '),
            read(Start),
            write('Enter End city: '),
            read(End),
            writeln(''),
            bestfs(Start, End, Path, 0, Cost),
            (
                nonvar(Cost) ->
                    write('Path: '),
                    writeln(Path),
                    write('Cost: '),
                    writeln(Cost)
            );
        writeln('Enter 1 or 2')
    ).

undo :- retract(distance(_, _, _)), fail. 
undo :- retract(key(_)), fail.
undo.

delH :- retract(h(_, _)), fail.
delH.