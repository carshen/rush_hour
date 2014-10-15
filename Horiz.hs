module Horiz (moves_horiz, moves_right, moves_left) where

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