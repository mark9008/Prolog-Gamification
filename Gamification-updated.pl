dim(3,3).
board([[1,1], [2,-1] , [3,0],
      [4,1], [5,0] , [6,1],
      [7,0] , [8,1] , [9,-1]]).


%main predicate
game(Start , Mingoal , Movementsallowed ):-
			 board(Board),
			 play(Start , Mingoal , Movementsallowed ,Board ,0).

% base case >> if we reach max number of movements
play([_ , Val],Mingoal,Move , B , Move):-
               format("Inputs: Minimumgoal = ~w" ,[Mingoal]),
	       nl,
	       print("the last state game state for that path is "),
	       nl,
	       print(B) ,
	       nl,
	       Val > Mingoal ->
	       (
		   format("Score is ~w and the Computer wins :)" ,[Val]),
		   nl
	       );
	        format("Score is ~w and the Computer fails :)" ,[Val]),
	        nl.
% base case >> if we reach goal
play([_ , Mingoal],Mingoal,_ , B , _):-
               format("Inputs: Minimumgoal = ~w" ,[Mingoal]),
	       nl,
	       print("the last state game state for that path is "),
	       nl,
	       print(B) ,
	       nl,
	       format("Score is ~w and the Computer wins :)" ,[Mingoal]),
	       nl.
%-----------------------------------------------------------------------------------
move([CurrIndex , CurrValue],[BestIndex ,_], Board , NumMovements ,NewMovements, NewStep , NewBoard ) :-
			dim(R ,C),
			Size is R * C ,
			BestIndex >= 1 ,BestIndex =< Size ->
			(

			addValue(CurrValue ,BestIndex , Board , [I,V] ,Tboard),

			NewStep = [I , V] ,
			NewMovements is NumMovements + 1,
			random(-1 , 2 , Random),
			replace(Tboard,CurrIndex ,Random , NewBoard)
			);
			false.


%--------------------------------------------------------------------------------------
play([Index ,Val] , Mingoal , Movementsallowed ,Board ,NumMovements)	:-
			% return newStep , number of movements , GameState
			dim(_,C),
			L is Index - 1 , RIGHT is Index +1 , Up is Index - C , Down is Index + C ,
			Moves = [L, RIGHT , Up , Down],

			checkmoves(Index,Moves,[] ,Board ,Availablemoves),
			getBestmove(Availablemoves ,[-1 , -500] ,BestMove),
			move([Index , Val],BestMove, Board ,NumMovements, New_Movements, NewStep , NewBoard),

			%print Gamestate
			printGameState(NewBoard),
			nl,
			play(NewStep,Mingoal ,Movementsallowed ,NewBoard ,New_Movements).



printGameState([]).
printGameState([H|T]) :-
		print(H),
		printGameState(T).



addValue(FirstVal , SecIndex , List, Step ,ResBoard) :-
			 getElementFromIndex(List,SecIndex,SecondVal),
			 NewVal is SecondVal + FirstVal,
			 replace(List, SecIndex,NewVal,ResBoard),
			 Step = [SecIndex , NewVal] .

getElementFromIndex([] , _ ,-10000).
getElementFromIndex([[_,V]|_],1,V).
getElementFromIndex([_|T],Index,Element):-
    I2 is Index-1,
    getElementFromIndex(T,I2,Element).



replace([[I,_]|T], 1, X, [[I,X]|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L ).



checkmoves(_ , [] ,Moves , _ ,Moves) .
checkmoves(Index ,[H|T], MovesList,Board,ResultList):-
			 %checking Element at the corner .
			 checkIndexValidity(Index ,H ,Result),
			 Result = 1 ->(
			 getElementFromIndex(Board ,H ,Val),
			 append([[H , Val]] , MovesList , Moves),
			 checkmoves(Index  , T , Moves ,Board , ResultList)
			 );
			 checkmoves(Index ,T, MovesList , Board ,ResultList).


getBestmove([] , Max , Max).
getBestmove([[Index , Val]|T] , [I2 ,Max] ,BestMove) :-
			 not(Val = -10000) ,Val > Max ->
			 (
			     Max1 is Val,
			     Index1 is Index ,
			     getBestmove(T ,[Index1 , Max1], BestMove)
			 );
			 getBestmove(T ,[I2 , Max] ,BestMove).


checkIndexValidity(Current , TargetIndex , 1 , Result) :- X is Current - TargetIndex , 1 = X , Result = -1 , !.
checkIndexValidity(Current , TargetIndex , 0 , Result) :- X is TargetIndex- Current , 1 = X , Result = -1 , !.
checkIndexValidity(_ , _ , _ , 1) .
checkIndexValidity(Current , TargetIndex, Result) :-
			 dim(_ , C),
			 N is Current mod C ,
			 checkIndexValidity(Current , TargetIndex ,N ,Result) .

