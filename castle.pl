% Base case, empty list, do nothing
printList([]).
printList([H | T]) :-
    write(H), nl,
    printList(T).

% Add your comments here
solveRooms(Castle, []).
solveRooms(Castle, [H | T]) :-
    findRoom(Castle, enter, H),
    solveRooms(Castle, T),
    getToExit(Castle, enter).

findRoom(Castle, Room, Room) :- write(Room), nl.
findRoom(Castle, FromRoom, Room) :-
    write(FromRoom), nl,
    room(Castle, FromRoom, ToRoom, Cost),
    findRoom(Castle, ToRoom, Room).

getToExit(Castle, exit) :- write(exit), nl.
getToExit(Castle, FromRoom) :-
    write(FromRoom), nl,
    room(Castle, FromRoom, ToRoom, Cost),
    getToExit(Castle, ToRoom).



% Add your comments here

