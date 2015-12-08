{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, (>>>), (>~>), returnVal, 
	Mutable, Pointer, Memory, runOp,
	StateOp,
	-- Weak Imports
	makeIntPointer, makeBoolPointer, Pointer(..),
	-- Testing Imports
	testMem, testMem, p1, p2, p3, p4, pList,
	testMem2, p21, p22, p23, p24
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.

-- TODO: Make pointerTest return Pointer Integer, Pointer Bool
pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
pointerTest n mem =
		((((P 100) :: Pointer Integer), ((P 500) :: Pointer Bool)),
		(snd (runOp ((def 100 (n + 3)) >>> (def 500 (n > 0))) mem)))

-- This was my origin pointer test function. 
-- The reason I commented it out was because I couldn't set
-- p3 to be a Pointer Integer. (It was always a Pointer Bool)
-- pointerTest n mem =
			-- let
			    -- p1 = 100
			    -- p2 = 500
			    -- op1 = (def p1 (n + 3)) :: StateOp (Pointer Integer)
			    -- op2 = (def p2 (n > 0)) :: StateOp (Pointer Bool)
			    -- result = op1 >~> \p3 ->
				-- op2 >~> \p4 ->
				-- get (p3 :: Pointer Integer) >>>
				-- get (p4 :: Pointer Bool)
		    -- in ((p3, p4), snd (runOp result mem))

			

-- Given two Pointers swap the values indicated by each pointer
swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap p1 p2 = let v1 = get p1
                 v2 = get p2
                 runOps = v1 >~> \res1 ->
                     v2 >~> \res2 ->
                     set p1 res2 >>>
                     set p2 res1
             in runOps			

-- which takes a list of pointers p1, ..., pn, with corresponding 
-- values v1, ..., vn, and sets p1's value to v2, p2's value to v3., etc., 
-- and pn's value to v1. This function should not change anything if its
-- argument has length less than 2.
swapCycle :: Mutable a => [Pointer a] -> StateOp ()
-- Swap cycle [] doesn't work TODO:
swapCycle [] = returnVal () :: StateOp ()
swapCycle [x] = swap x x
swapCycle (x:xs) = swap x (head xs) >>>
                   swapCycle xs

 

-- Part 1 Code
-- pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
-- pointerTest n mem = let 
						-- intPointer = def mem 100 (n + 3)
						-- boolPointer = def (snd intPointer) 500 (n > 0)
					-- in (((fst intPointer), (fst boolPointer)), (snd boolPointer))

