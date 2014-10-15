import Solver
import Horiz

tests :: [(Board,Board)]

e :: String -> Board
e = moves_horiz

f :: String -> Board
f = moves_right

g :: String -> Board
g = moves_left

tests = [
	(e "" , []),
	(e "aaabb", []),
	(e "aa-", ["-aa"]),
	(e "-aa", ["aa-"]),
	(e "-aaa", ["aaa-"]),
	(e "aaa-", ["-aaa"]),
	(e "bbb-aaa-", ["-bbbaaa-","bbb--aaa","bbbaaa--"]),

	(f "" ,[]),
	(f "-" , []),
	(f "BB" , []),
	(f "BB-" , ["-BB"]),
	(f "aa--" , ["-aa-"]),
	(f "aa-bb-c" , ["-aabb-c","aa--bbc"]),
	(f "aa-bb-cc-dd" , ["-aabb-cc-dd","aa--bbcc-dd","aa-bb--ccdd"]),
	(f "aa-bb-cc-dd-" , ["-aabb-cc-dd-","aa--bbcc-dd-","aa-bb--ccdd-","aa-bb-cc--dd"]),
	(f "aa-bbC-c", ["-aabbC-c"]),
	
	-- to the right to the right
	(f "aa----bc", ["-aa---bc"]),
	(f "-aa---bc", ["--aa--bc"]),
	(f "--aa--bc", ["---aa-bc"]),
	(f "---aa-bc", ["----aabc"]),
	(f "----aabc", []),
	
	-- to the left to the left
	(g "----aabc", ["---aa-bc"]),
	(g "---aa-bc", ["--aa--bc"]),
	(g "--aa--bc", ["-aa---bc"]),
	(g "-aa---bc", ["aa----bc"]),
	(g "aa----bc", []),
	
	(g "-aa-bb-cc-dd", ["aa--bb-cc-dd","-aabb--cc-dd","-aa-bbcc--dd", "-aa-bb-ccdd-"])
	]
	
run_tests :: [(Board,Board)] -> Board
run_tests list = map (\(x) -> if (fst x) == (snd x) then "Pass" else "Fail") list

runall :: Board
runall = run_tests tests

tests_whole :: [([Board],[Board])]
tests::whole = [
	(horizontal_moves_helper ["dd--dd-","ccc-ccc","ccc-aa-"], [["-dd-dd-","-cccccc","-cccaa-"],["-dd-dd-","-cccccc","ccc--aa"],["-dd-dd-","-cccccc","cccaa--"],["-dd-dd-","cccccc-","-cccaa-"],["-dd-dd-","cccccc-","ccc--aa"],["-dd-dd-","cccccc-","cccaa--"],["dd---dd","-cccccc","-cccaa-"],["dd---dd","-cccccc","ccc--aa"],["dd---dd","-cccccc","cccaa--"],["dd---dd","cccccc-","-cccaa-"],["dd---dd","cccccc-","ccc--aa"],["dd---dd","cccccc-","cccaa--"],["dd-dd--","-cccccc","-cccaa-"],["dd-dd--","-cccccc","ccc--aa"],["dd-dd--","-cccccc","cccaa--"],["dd-dd--","cccccc-","-cccaa-"],["dd-dd--","cccccc-","ccc--aa"],["dd-dd--","cccccc-","cccaa--"]])
]