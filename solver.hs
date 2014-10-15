module Solver (moves_horiz, moves_right) where

-- Rush Hour

-- Takes the initial board and returns a list of boards, the path.
-- Assumption: the board is at least a 3x2 matrix. Note we don't assume the board always has to be size 6,
-- But it will do the job for a 6x6 matrix.

rush_hour :: [String] -> [[String]]
rush_hour init_state = reverse(solver [init_state] [])

-- Takes a list of unexplored boards, and the list of boards experienced so far. 
solver :: [[String]] -> [[String]] -> [[String]]
solver unexplored path
	| null unexplored		= []
	| elem (head unexplored) path	= solver (tail unexplored) path
	| is_goal (head unexplored)	= ((head unexplored):path)
	| (not (null result))		= result
	| otherwise			= solver (tail unexplored) path
	where result = solver (generate_moves (head unexplored)) ((head unexplored):path)
	
-- Checks if 'XX' present in last 2 columns of the 3rd row on the board.
is_goal :: [String] -> Bool
is_goal board = (special_car_at_goal (get_row board 3) (length (head board)))

-- Takes a board and generates a list of boards 1 move away.
generate_moves :: [String] -> [[String]]
generate_moves board = (generate_vertical_moves board) ++ (generate_horizontal_moves board)

-- Takes a board and generates a list of boards where each board is a valid vertical move.
generate_vertical_moves :: [String] -> [[String]]
generate_vertical_moves board = [board]

-- Takes a board and generates a list of boards where each board is a valid horizontal move.
generate_horizontal_moves :: [String] -> [[String]]
generate_horizontal_moves board = [board]

drop_last :: [a] -> [a]
drop_last []			= []
drop_last (x:xs)
	| null xs			= []
	| (null (tail xs))	= [x]
	| otherwise			= x:(drop_last xs)
	
is_car :: Char -> Bool
is_car '-'	= False
is_car _	= True

moves_horiz :: [Char] -> [[Char]]
moves_horiz chars = (moves_right chars)
-- ++ (moves_left chars)
	
--Get the possible moves to the right on one horizontal line
-- "AA-" -> "-AA"
moves_right :: [Char] -> [[Char]]
moves_right arr = moves_helper
		-- Car matching
		(\ v w x ->
			-- Check if first 2 are equal, cars, and 3rd is empty
			(v == w) && (is_car v) && (is_car w) && x == '-'
		)
		-- Truck matching (should be done before car)
		(\ v w x y ->
			-- Check if first 2 are equal, cars, and 3rd is empty
			(v == w) && (w == x) && (is_car v) && (is_car w) && (is_car x) && y == '-'
		)
		-- Car generation
		(\ v w x rest ->
			-- Compose empty with the 2 car chars and the rest of the list
			(x:(v:(w:rest)))
		)
		-- Truck generation
		(\ v w x y rest ->
			-- Compose empty with the 3 car chars and the rest of the list
			(y:(v:(w:(x:rest))))
		)
		""
		arr

-- mf = matching function for car, truck_mf for truck
moves_helper ::
	(Char -> Char -> Char -> Bool) -> --Match car
	(Char -> Char -> Char -> Char -> Bool) -> --Match truck
	(Char -> Char -> Char -> [Char] -> [Char]) -> --Gen car
	(Char -> Char -> Char -> Char -> [Char] -> [Char]) -> --Gen truck
	[Char] -> --beginning
	[Char] -> --string
	[[Char]]
moves_helper _ _ _ _ _ [] = []
moves_helper car_mf truck_mf gen_car gen_truck beg (v:(w:(x:(y:z))))
	-- Thank god it's only one move at a time
	-- So like "[Car][Car <same as first>]-" should generate "-[Car][Car <same as first>]"
	| truck_mf v w x y =
		(beg ++ (gen_car v w x (y:z))) :
			(moves_helper car_mf truck_mf gen_car gen_truck (beg ++ (v:(w:[x]))) (y:z) )
	| car_mf v w x =
		(beg ++ (gen_car v w x (y:z))) :
			(moves_helper car_mf truck_mf gen_car gen_truck (beg ++ (v:(w:[x]))) (y:z) )
			
	| otherwise = moves_helper car_mf truck_mf gen_car gen_truck (beg ++ [v]) (w:(x:(y:z)))
	
	
moves_helper car_mf truck_mf gen_car gen_truck beg (v:(w:(x:y)))
	| car_mf v w x =
		(beg ++ (gen_car v w x y)) :
			(moves_helper car_mf truck_mf gen_car gen_truck (beg ++ (v:(w:[x]))) y)
			
	| otherwise = moves_helper car_mf truck_mf gen_car gen_truck (beg ++ [v]) (w:(x:y))
moves_helper _ _ _ _ _ _ = []


-- Misc. helpers
-- Return the nth row of the board.
get_row :: [String] -> Int -> String
get_row board n
	| n==1		= (head board)
	| otherwise	= (get_row (tail board) (n-1))

-- Return True if there is a special car at the goal position in the given row of length n.
special_car_at_goal :: String -> Int -> Bool
special_car_at_goal row n
	| (null (tail row))			= False
	| n==2 && (special_car_at_head row)	= True
	| otherwise				= special_car_at_goal (tail row) (n-1)
	
-- Return True if special car at the head of the list where list size >= 2.
special_car_at_head :: String -> Bool
special_car_at_head row = (head row)=='X' && (head (tail row))=='X'
