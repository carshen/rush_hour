module Solver (Board, Line, generate_moves, generate_horizontal_moves, generate_vertical_moves) where
import Horiz 
import Data.List

-- Rush Hour

-- Takes the initial board and returns a list of boards, the path.
-- Assumption: the board is at least a 3x2 matrix. Note we don't assume the board always has to be size 6,
-- But it will do the job for a 6x6 matrix.

type Board = [String]
type Line = String

pretty board = mapM_ putStrLn board
pretty_board boards = mapM_ pretty boards

rush_hour :: Board -> [Board]
rush_hour init_state = reverse(solver [init_state] [])

-- Takes a list of unexplored boards, and the list of boards experienced so far. 
solver :: [Board] -> [Board] -> [Board]
solver unexplored path
	| null unexplored		= []
	| elem (head unexplored) path	= solver (tail unexplored) path
	| is_goal (head unexplored)	= (head unexplored):path
	| (not (null result))		= result
	| otherwise			= solver (tail unexplored) path
	where result = solver (generate_moves (head unexplored)) ((head unexplored):path)
	
-- Checks if 'XX' present in last 2 columns of the 3rd row on the board.
is_goal :: Board -> Bool
is_goal board = (special_car_at_goal (get_row board 3) (length (head board)))

-- Takes a board and generates a list of boards 1 move away.
generate_moves :: Board -> [Board]
generate_moves board =	generate_moves_helper board (generate_vertical_moves board) ++
						generate_moves_helper board (generate_horizontal_moves board)

-- If nothing can be done then generate_horizontal_moves will return the initial board;
-- This function simply removes that board
generate_moves_helper :: Board -> [Board] -> [Board]
generate_moves_helper initialBoard (x:[]) = if x == initialBoard then [] else (x:[])
generate_moves_helper _ boards = boards

-- Takes a board and generates a list of boards where each board is a valid vertical move.
generate_vertical_moves :: Board -> [Board]
generate_vertical_moves board = map (\ rotated_board -> transpose rotated_board) (generate_horizontal_moves (transpose board))

-- Takes a board and generates a list of boards where each board is a valid horizontal move.
generate_horizontal_moves :: Board -> [Board]
generate_horizontal_moves [] = []
generate_horizontal_moves (line:[])
	| null line_moves = [[line]]
	| otherwise = map (\line -> [line]) line_moves
	where line_moves = moves_horiz line
	
generate_horizontal_moves (line:(next_line:rest)) 
 	| null line_moves = map (\ board -> line:board) next_boards
	| otherwise =
		concat
		(
			map
			(\ line ->
				map (\ board -> line:board) next_boards
			)
			line_moves
		)
 	where
 		line_moves = moves_horiz line
 		next_boards = generate_horizontal_moves (next_line:rest)
		
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
	| n == 2 && (special_car_at_head row)	= True
	| otherwise				= special_car_at_goal (tail row) (n-1)
	
-- Return True if special car at the head of the list where list size >= 2.
special_car_at_head :: String -> Bool
special_car_at_head row = (head row) == 'X' && (head (tail row)) == 'X'
