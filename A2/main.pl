:- [library(csv)].

:- dynamic distance/3.
:- dynamic key/1.

filename('roaddistance.csv').

readfile(File):-
    undo,
    filename(File),
    forall(readrow(File, Row), storerow(Row)).

readrow(File, Row) :-
    csv_read_file_row(File, Row, [strip(true), convert(true)]),
    writeln(readrow(Row)).

storerow(Row) :-
    Row =.. [row|Cols],
    (   key(ColKeys) ->  Cols = [RowKey|Samples],
        maplist(storesample(RowKey), ColKeys, Samples)
    ;   assert(key(Cols))
    ).

storesample(RowKey, ColKey, Value) :-
    assert(distance(RowKey, ColKey, Value)).

undo :- retract(distance(_, _, _)), fail. 
undo :- retract(key(_)), fail.
undo.