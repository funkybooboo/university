% First castle for testing
% The castle is a set of room facts of the form
% room(Castle, FromRoom, ToRoom, cost).
room(dunstanburgh, enter, foyer, 1).
room(dunstanburgh, foyer, livingRoom, 1).
room(dunstanburgh, foyer, hall, 2).
room(dunstanburgh, hall, kitchen, 4).
room(dunstanburgh, hall, garage, 3).
room(dunstanburgh, kitchen, exit, 1).

% Second castle for testing
room(windsor, enter, foyer, 1).
room(windsor, foyer, hall, 2).
room(windsor, foyer, dungeon, 1).
room(windsor, hall, throne, 1).
room(windsor, hall, stairs, 4).
room(windsor, stairs, dungeon, 3).
room(windsor, throne, stairs, 1).
room(windsor, dungeon, escape, 5).
room(windsor, escape, exit, 1).

% Third castle for testing
room(alnwick, enter, foyer, 1).
room(alnwick, foyer, hall, 2).
room(alnwick, hall, throne, 1).
room(alnwick, hall, stairs, 4).
room(alnwick, stairs, dungeon, 3).
room(alnwick, dungeon, foundry, 5).
room(alnwick, foyer, passage, 1).
room(alnwick, passage, foundry, 1).
room(alnwick, foundry, exit, 4).

test1 :- write('solveRoomsWithinCost(dunstanburgh, 8)'), nl,
           solveRoomsWithinCost(dunstanburgh, 8).
test2 :- write('solveRoomsWithinCost(windsor, 13)'), nl,
           solveRoomsWithinCost(windsor, 13).
test3 :- write('solveRoomsWithinCost(alnwick, 15)'), nl,
           solveRoomsWithinCost(alnwick, 15).
test4 :- write('solveRooms(dunstanburgh, [foyer, kitchen])'), nl,
           solveRooms(dunstanburgh, [foyer, kitchen]).
test5 :- write('solveRooms(windsor, [stairs])'), nl,
           solveRooms(windsor, [stairs]).
test6 :- write('solveRooms(alnwick, [foyer, hall])'), nl,
           solveRooms(alnwick, [foyer, hall]).
test7 :- write('solveRooms(alnwick, [foyer, passage])'), nl,
           solveRooms(alnwick, [foyer, passage]).
test8 :- write('fails: solveRooms(alnwick, [foyer, throne, escape])'), nl,
           solveRooms(alnwick, [foyer, throne, passage]).
test9 :- write('fails: solveRoomsWithinCost(alnwick, 4)'), nl,
           solveRoomsWithinCost(alnwick, 4).
