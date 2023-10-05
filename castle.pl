% Base case, empty list, do nothing
printList([]).
printList([H | T]) :-
    write(H), nl,
    printList(T).

% Add your comments here
solveRooms(Castle, []).
solveRooms(Castle, L) :-
    move(Castle, enter, L, [enter]).

move(Castle, exit, [], B) :- true.
move(Castle, exit, L, B) :- false.
move(Castle, FromRoom, L, B) :-
    room(Castle, FromRoom, ToRoom, Cost),
    checkList(ToRoom, L, NL),
    move(Castle, ToRoom, NL).

checkList(Room, [], L).
checkList(Room, L, NL) :-
    member(Room, L),
    removeItem(Room, L, NL);
    NL = L.

removeItem(Room, [L], []).
removeItem(Room, [Room | L], L).
removeItem(Room, [H|L], [H|NL]) :- removeItem(Room, L, NL).

% Add your comments here
%solveRoomsWithinCost(Castle, CostLimit) :-
%    room(Castle, enter, ToRoom, Cost),
