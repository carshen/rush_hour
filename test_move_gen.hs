import Solver

tests :: [([String],[String])]

tests = [
	(pat3_match "" ,[]),
	(pat3_match "-" , []),
	(pat3_match "BB" , []),
	(pat3_match "BB-" , ["-BB"]),
	(pat3_match "aa--" , ["-aa-"]),
	(pat3_match "aa-bb-c" , ["-aabb-c","aa--bbc"]),
	(pat3_match "aa-bbC-c" , ["-aabbC-c"]),
	(pat3_match "-aa---bc" , ["--aa--bc"]),
	(pat3_match "--aa--bc" , ["---aa-bc"])
	
	]
	
run_tests :: [([String],[String])] -> [String]
run_tests list = map (\(x) -> if (fst x) == (snd x) then "Pass" else "Fail") list

runall :: [String]
runall = run_tests tests  