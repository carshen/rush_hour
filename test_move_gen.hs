import Solver

tests :: [([String],[String])]

e :: String -> [String]
e = moves_horiz

f :: String -> [String]
f = moves_right

tests = [
	(e "" , []),
	(e "aa-", ["-aa"]),
	(e "-aa", ["aa-"]),

	(f "" ,[]),
	(f "-" , []),
	(f "BB" , []),
	(f "BB-" , ["-BB"]),
	(f "aa--" , ["-aa-"]),
	(f "aa-bb-c" , ["-aabb-c","aa--bbc"]),
	(f "aa-bb-cc-dd" , ["-aabb-cc-dd","aa--bbcc-dd","aa-bb--ccdd"]),
	(f "aa-bb-cc-dd-" , ["-aabb-cc-dd-","aa--bbcc-dd-","aa-bb--ccdd-","aa-bb-cc--dd"]),
	(f "aa-bbC-c", ["-aabbC-c"]),
	(f "aa----bc", ["-aa---bc"]),
	(f "-aa---bc", ["--aa--bc"]),
	(f "--aa--bc", ["---aa-bc"]),
	(f "---aa-bc", ["----aabc"]),
	(f "----aabc", [])
	]
	
run_tests :: [([String],[String])] -> [String]
run_tests list = map (\(x) -> if (fst x) == (snd x) then "Pass" else "Fail") list

runall :: [String]
runall = run_tests tests  