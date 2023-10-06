% printList goal: print a given list
printList([]). % base case, empty list, do nothing
printList([H | T]) :- % there is at least one thing in the list, take the first item out of the list
    write(H), nl, % print the first item
    printList(T). % print the rest

% SolveRooms goal: head
solveRooms(Castle, []). % used to catch if the user doesnt spesify rooms
solveRooms(Castle, L) :- % there is at last one room in the list
    solveRooms(Castle, enter, L, E), % start the resurtion
    append([enter], E, X), % get the list ready to print
    printList(X). % print list
% SolveRooms goal: helpers
solveRooms(Castle, exit, [], E) :- true. % base case, if we got to the exit and the Room List L is empty then start breaking out of recurtion
solveRooms(Castle, exit, L, E) :- false. % base case, if we got to the exit and there is still rooms in the Room List L then backtrack
solveRooms(Castle, FromRoom, L, E) :- % defult case, there are still rooms in the Room List L and we havent gotten to the exit
    room(Castle, FromRoom, ToRoom, Cost), % get our next room
    checkList(ToRoom, L, NL), % check if the next room is in the Room List L, if it is then remove it
    solveRooms(Castle, ToRoom, NL, TempE), % recursivly call with the next room and updated Room List
    append([ToRoom], TempE, E). % after we have found a solution collect all the rooms that have been truly visted
% checkList goal: see if I can remove a room from the L list
checkList(Room, [], L). % if the Room list is empty return the empty list
checkList(Room, L, NL) :- % check to see if Room is in L and return NL
    member(Room, L), % check if its a member
    removeItem(Room, L, NL); % if it is then remove the item
    NL = L. % if its not then return NL
% removeItem goal: remove the room
removeItem(Room, [L], []). % is there is only one item in the list then it has to be the room, return an empty list
removeItem(Room, [Room | L], L). % if room is at the front of the list then return the tail of the list
removeItem(Room, [H|L], [H|NL]) :- removeItem(Room, L, NL). % resursivly find where room is and store the rest of the rooms other then the room and return that new list

% solveRoomsWithinCost goal: head
solveRoomsWithinCost(Castle, CostLimit) :- % for any cost test to see if there is a path from enter to exit with a cost of under or equal to the cost limit
    solveRoomsWithinCost(Castle, CostLimit, enter, EndCost, E), % start the resurtion
    format("Cost is ~w within limit of ~w", [EndCost, CostLimit]), nl, % print out the awnsers
    append([enter], E, X), % get the list ready to print
    printList(X). % print list
% solveRoomsWithinCost goal: helpers
solveRoomsWithinCost(Castle, CostLimit, exit, 0, []):- CostLimit >= 0. % base case, if there is a remaining amount in the limit and the exit is reached then break out of resuriton, also start accumilating the real totals
solveRoomsWithinCost(Castle, CostLimit, FromRoom, EndCost, E) :- % defualt case, we havent gotten to the exit and there is still some left over "costlimit" amout so we can keep looking
    room(Castle, FromRoom, ToRoom, Cost), % get the next room and cost
    AccCostLimit is CostLimit - Cost, % get the new cost limit
    AccCostLimit >= 0, % check bounds
    solveRoomsWithinCost(Castle, AccCostLimit, ToRoom, AccEndCost, AccE), % recursive call with new room and costlimit
    EndCost is AccEndCost + Cost, % accumilate end cost for return value with all the rooms we really went to
    append([ToRoom], AccE, E). % after we have found a solution collect all the rooms that have been truly visted
