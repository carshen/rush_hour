module Solver where
import Horiz

-- Rush Hour

-- Takes the initial board and returns a list of boards, the path.
-- Assumption: the board is at least a 3x2 matrix. Note we don't assume the board always has to be size 6,
-- But it will do the job for a 6x6 matrix.

type Board = [String]
type Line = String

rush_hour :: Board -> [Board]
rush_hour init_state = reverse(solver [init_state] [])

-- Takes a list of unexplored boards, and the list of boards experienced so far. 
solver :: [Board] -> [Board] -> [Board]
solver unexplored path
	| null unexplored		= []
	| elem (head unexplored) path	= solver (tail unexplored) path
	| is_goal (head unexplored)	= ((head unexplored):path)
	| (not (null result))		= result
	| otherwise			= solver (tail unexplored) path
	where result = solver (generate_moves (head unexplored)) ((head unexplored):path)
	
-- Checks if 'XX' present in last 2 columns of the 3rd row on the board.
is_goal :: Board -> Bool
is_goal board = (special_car_at_goal (get_row board 3) (length (head board)))

-- Takes a board and generates a list of boards 1 move away.
generate_moves :: Board -> [Board]
generate_moves board = (generate_vertical_moves board) ++ (generate_horizontal_moves board)

-- Takes a board and generates a list of boards where each board is a valid vertical move.
generate_vertical_moves :: Board -> [Board]
generate_vertical_moves board = [board]

-- Takes a board and generates a list of boards where each board is a valid horizontal move.
-- O(2^(nm)) where n is number of lines m is length of line

-- Prepend a line to a board
prepend_to_board :: Board -> Line -> Board
prepend_to_board board line = line: board

generate_horizontal_moves :: Board -> [Board]
generate_horizontal_moves [] = []
generate_horizontal_moves x = horizontal_moves_helper x

horizontal_moves_helper :: Board -> [Board]
horizontal_moves_helper [] = []
horizontal_moves_helper (line:[])
	| null line_moves = [[line]]
	| otherwise = map (\line -> [line]) line_moves
	where line_moves = moves_horiz line
	
horizontal_moves_helper (line:(next_line:rest)) 
 	| null line_moves = map (\ board -> prepend_to_board board line) next_boards
	| otherwise =
		concat
		(
		map
			(\ line ->
				map (\ board -> prepend_to_board board line) next_boards
			)
		line_moves
		)
 	where
 		line_moves = moves_horiz line
 		next_boards = horizontal_moves_helper (next_line:rest)
		
-- Misc. helpers
-- Return the nth row of the board.
get_row :: Board -> Int -> String
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
