:- [library(csv)].

:- dynamic distance/3.
:- dynamic key/1.

filename('roaddistance.csv').

readfile(File):-
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

path(Start, End , Path, InitCost, FinalCost):-
    Start == End -> write('Same start and end point');
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


undo :- retract(distance(_, _, _)), fail. 
undo :- retract(key(_)), fail.
undo.