:- [library(csv)].

:- dynamic distance/3.
:- dynamic key/1.

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
    (   key(ColKeys) ->  Cols = [RowKey|Samples],
        maplist(storesample(RowKey), ColKeys, Samples)
    ;   assert(key(Cols))
    ).

storesample(RowKey, ColKey, Value):-
    assert(distance(RowKey, ColKey, Value)),
    assert(distance(ColKey, RowKey, Value)).

/*
Depth First Search
*/

dfs(Start, End , Path, InitCost, FinalCost):-
    Start == End -> writeln('Same start and end point');
    path(Start, End, [Start], L, InitCost, FinalCost),
    reverse(L,Path).

path(Cur, Next, Path, [Next|Path], Cost, FinalCost):-
   distance(Cur, Next, Dist),
   FinalCost is Cost + Dist.

path(Cur, Next, L, Path, Cost, FinalCost):-
   distance(Cur, Any, Dist), 
   \+ member(Any, L),          % avoid visited nodes
   NewCost is Cost + Dist,
   path(Any, Next, [Any|L], Path, NewCost, FinalCost).

/*
Best First Search
*/

bestfs(Start, End, Path, InitCost, FinalCOst):-
    Start == End -> writeln('Same start and end point');
    writeln('best fs').

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