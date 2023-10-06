% Base case, empty list, do nothing
printList([]).
printList([H | T]) :-
    write(H), nl,
    printList(T).

% SolveRooms predicate, head
solveRooms(Castle, []).
solveRooms(Castle, L) :-
    solveRooms(Castle, enter, L, E),
    append([enter], E, X),
    printList(X).
% SolveRooms predicate, helpers
solveRooms(Castle, exit, [], E) :- true.
solveRooms(Castle, exit, L, E) :- false.
solveRooms(Castle, FromRoom, L, E) :-
    room(Castle, FromRoom, ToRoom, Cost),
    checkList(ToRoom, L, NL),
    solveRooms(Castle, ToRoom, NL, TempE),
    append([ToRoom], TempE, E).
% checkList predicate, see if I can remove a room from the L list
checkList(Room, [], L).
checkList(Room, L, NL) :-
    member(Room, L),
    removeItem(Room, L, NL);
    NL = L.
% removeItem predicate, remove the room
removeItem(Room, [L], []).
removeItem(Room, [Room | L], L).
removeItem(Room, [H|L], [H|NL]) :- removeItem(Room, L, NL).

% solveRoomsWithinCost predicate, head
solveRoomsWithinCost(Castle, CostLimit) :-
    solveRoomsWithinCost(Castle, CostLimit, enter, EndCost, E),
    format("Cost is ~w within limit of ~w", [EndCost, CostLimit]), nl,
    append([enter], E, X),
    printList(X).
% solveRoomsWithinCost predicate, helpers
solveRoomsWithinCost(Castle, CostLimit, exit, 0, []):- CostLimit >= 0.
solveRoomsWithinCost(Castle, CostLimit, FromRoom, EndCost, E) :-
    room(Castle, FromRoom, ToRoom, Cost),
    AccCostLimit is CostLimit - Cost,
    AccCostLimit >= 0,
    solveRoomsWithinCost(Castle, AccCostLimit, ToRoom, AccEndCost, AccE),
    EndCost is AccEndCost + Cost,
    append([ToRoom], AccE, E).
