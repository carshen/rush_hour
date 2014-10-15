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
	
-- Is this a car/truck character?
is_car :: Char -> Bool
is_car '-'	= False
is_car _	= True

-- All the moves for one horizontal line
moves_horiz :: [Char] -> [[Char]]
moves_horiz chars = (moves_right chars) ++ (moves_left chars)

-- Do these 3 characters represent a car and a blank space?
car_match :: Char -> Char -> Char -> Bool
car_match c1 c2 blank = 
			(c1 == c2) &&
			(is_car c1) && (is_car c2) &&
			blank == '-'
			
-- Do these 3 characters represent a truck and a blank space?
truck_match :: Char -> Char -> Char -> Char -> Bool
truck_match c1 c2 c3 blank = 
			(c1 == c2) && (c2 == c3) &&
			(is_car c1) && (is_car c2) && (is_car c3) &&
			blank == '-'
	
--Get the possible moves to the right on one horizontal line
-- "AA-" -> "-AA"
moves_right :: [Char] -> [[Char]]
moves_right arr = moves_helper
		-- Car matching
		(\ c1 c2 blank ->
			car_match c1 c2 blank
		)
		-- Truck matching (should be done before car)
		(\ c1 c2 c3 blank ->
			truck_match c1 c2 c3 blank
		)
		-- Car generation
		(\ c1 c2 blank rest ->
			-- Compose blank, the 2 car chars, and the rest of the list
			(blank:(c1:(c2:rest)))
		)
		-- Truck generation
		(\ c1 c2 c3 blank rest ->
			-- Compose blank, the 3 car chars, and the rest of the list
			(blank:(c1:(c2:(c3:rest))))
		)
		""
		arr
		
--Get the possible moves to the right on one horizontal line
-- "AA-" -> "-AA"
moves_left :: [Char] -> [[Char]]
moves_left arr = moves_helper
		-- Car matching
		(\ blank c1 c2 ->
			car_match c1 c2 blank
		)
		-- Truck matching (should be done before car)
		(\ blank c1 c2 c3 ->
			truck_match c1 c2 c3 blank
		)
		-- Car generation
		(\ blank c1 c2 rest ->
			-- Compose 2 car chars, blank, and the rest of the list
			(c1:(c2:(blank:rest)))
		)
		-- Truck generation
		(\ blank c1 c2 c3 rest ->
			-- Compose 3 car chars, blank, and the rest of the list
			(c1:(c2:(c3:(blank:rest))))
		)
		""
		arr

-- mf = matching function for car, truck_mf for truck
-- gf = generating function for car, truck_gf for truck
moves_helper ::
	(Char -> Char -> Char -> Bool) -> -- Car mf
	(Char -> Char -> Char -> Char -> Bool) -> -- Truck mf
	(Char -> Char -> Char -> [Char] -> [Char]) -> -- Car gf
	(Char -> Char -> Char -> Char -> [Char] -> [Char]) -> -- Truck gf
	[Char] -> --beginning
	[Char] -> --string
	[[Char]]
moves_helper _ _ _ _ _ [] = []
moves_helper car_mf truck_mf car_gf truck_gf beg (v:(w:(x:(y:z))))
	-- Check trucks first before cars so they don't get turned into cars
	| truck_mf v w x y =
		(beg ++ (truck_gf v w x y z)) :
			(moves_helper car_mf truck_mf car_gf truck_gf (beg ++ (v:(w:[x]))) (y:z) )
	| car_mf v w x =
		(beg ++ (car_gf v w x (y:z))) :
			(moves_helper car_mf truck_mf car_gf truck_gf (beg ++ (v:(w:[x]))) (y:z) )
			
	| otherwise = moves_helper car_mf truck_mf car_gf truck_gf (beg ++ [v]) (w:(x:(y:z)))
	
moves_helper car_mf truck_mf car_gf truck_gf beg (v:(w:(x:y)))
	| car_mf v w x =
		(beg ++ (car_gf v w x y)) :
			(moves_helper car_mf truck_mf car_gf truck_gf (beg ++ (v:(w:[x]))) y)
			
	| otherwise = moves_helper car_mf truck_mf car_gf truck_gf (beg ++ [v]) (w:(x:y))
	
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
