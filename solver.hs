-- Rush Hour

-- Takes the initial board and returns a list of boards, the path.
rush_hour :: [String] -> [[String]]
rush_hour init_state = solver [init_state] []

-- Takes a list of unexplored boards, and the list of boards experienced so far. 
solver :: [[String]] -> [[String]] -> [[String]]
solver unexplored path
	| null unexplored		= []
--	| Check we haven't visited this path. 
	| is_goal (head unexplored)	= ((head unexplored):path)
	| (not (null result))		= result
	| otherwise			= solver (tail unexplored) path
	where result = solver (generate_moves (head unexplored)) ((head unexplored):path)
	
-- Checks if 'XX' present in last 2 columns of the 3rd row on the board.
is_goal :: [String] -> Bool
is_goal board = True

-- Takes a board and generates a list of boards 1 move away.
generate_moves :: [String] -> [[String]]
generate_moves board = [board]
