type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up :: MyState -> MyState
up (S (0,_) _ _ _) = Null
up (S (x,y) cells str m ) = (S (x-1,y) cells "up" (S (x,y) cells str m))

down :: MyState -> MyState
down (S (3,_) _ _ _) = Null
down (S (x,y) cells str m ) = (S (x+1,y) cells "down" (S (x,y) cells str m))

left :: MyState -> MyState
left (S (_,0) _ _ _) = Null
left (S (x,y) cells str m ) = (S (x,y-1) cells "left" (S (x,y) cells str m))

right :: MyState -> MyState
right (S (_,3) _ _ _) = Null
right (S (x,y) cells str m) = (S (x,y+1) cells "right" (S (x,y) cells str m))


checkIfCellExists :: Cell -> [Cell] -> Bool
checkIfCellExists cell [] = False
checkIfCellExists (x,y) ((a,b):t) = if (x==a && y==b)
									then True
									else (checkIfCellExists (x,y) t)

removeElem :: Cell -> [Cell] -> [Cell]
removeElem cell [] = []
removeElem (x,y) ((z,m):t) = if(x==z && m==y)
							then t
							else (z,m) : (removeElem (x,y) t)

collect:: MyState -> MyState
collect (S (x,y) arr str  m ) = if(checkIfCellExists (x,y) arr)
  										then (S (x,y) (removeElem (x,y) arr) "collect" (S (x,y) arr str m ))
  										  else Null



nextMyStatesHelper :: MyState -> [MyState]
nextMyStatesHelper x =[(up x),(down x),(left x),(right x),(collect x)]

removeNull :: [MyState] -> [MyState]
removeNull [] = []
removeNull (h:t) = if(h==Null) then removeNull t
                               else h:(removeNull t)
nextMyStates :: MyState -> [MyState]
nextMyStates Null = []
nextMyStates x = removeNull (nextMyStatesHelper x)

isGoal :: MyState -> Bool
isGoal (S a arr m  n) | (arr == []) = True
                      | otherwise = False

--isGoal (S _ [(x,y)] _ _) = False

search::[MyState]->MyState
search (h:t) | isGoal h = h
             | otherwise = search (t ++ (nextMyStates h))

reverseL :: [String] -> [String]
reverseL  [] = []
reverseL  xs = last xs : reverseL (init xs)

getStrings :: MyState -> [String]
getStrings Null = []
getStrings (S (x,y) arr str  m ) =str : (getStrings m)

constructSolution:: MyState ->[String]


constructSolution s =reverseL(filter (not . null) (getStrings (search [s])))

solve :: Cell->[Cell]->[String]
solve m arr = constructSolution (S m arr "" Null)
