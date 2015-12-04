{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, (>>>), (>~>), returnVal, 
	Mutable, Pointer, Memory, runOp,
	-- Testing Imports
	makePointer, makePointer2, testMem
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.


pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
pointerTest n mem =
			let p1 = 100
			    p2 = 500
			    op1 = def p1 (n + 3)
			    op2 = def p2 (n > 0)
			    result = op1 >~> \p3 ->
			             op2 >~> \p4 ->
			             get (makePointer p3) >>>
			             get (makePointer2 p2)
			in ((makePointer p1, makePointer2 p2), snd (runOp result mem))
			
-- Part 1 Code
-- pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
-- pointerTest n mem = let 
						-- intPointer = def mem 100 (n + 3)
						-- boolPointer = def (snd intPointer) 500 (n > 0)
					-- in (((fst intPointer), (fst boolPointer)), (snd boolPointer))

