{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
	(>>>), (>~>), returnVal, runOp,
    Memory, Pointer, 
	-- Testing Exports
	inList, getInt, getBool, makePointer, makePointer2,
	testMem
    )
    where

import AList (AList, lookupA, insertA, updateA) 
-- A type representing the possible values stored in memory.
data Value = IntVal Integer  |
             BoolVal Bool   
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value
-- Same as [(Integer, StateOp)]

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

inList :: Eq a => AList a b -> a -> Bool
inList alist key =  if (null alist)
                        then False
                        else 
                            if (fst (head alist)) == key
                                then True
                                else inList (tail alist) key    
            

getInt :: Value -> Integer
getInt (IntVal x) = x
getInt _ = error "Invalid Type"

getBool :: Value -> Bool
getBool (BoolVal x) = x
getBool _ = error "Invalid Type"

bp1 :: Pointer Bool
bp1 = (P 3)

bp2 :: Pointer Bool
bp2 = (P 4)

testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]
testBool = [(3, BoolVal True), (4, BoolVal False)]
testMem2 = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]
testBool2 = [(3, BoolVal True), (4, BoolVal False)]

makePointer :: Integer -> Pointer Integer
makePointer x = (P x)

makePointer2 :: Integer -> Pointer Bool
makePointer2 x = (P x)

data StateOp a = StateOp (Memory -> (a, Memory))

testOp :: StateOp Integer
testOp = StateOp (\mem -> (5, mem))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- f :: Integer -> StateOp Integer
-- f x =
    -- def x 4 >~> \p1 ->
	-- def (x + 5) 20 >>>
	-- get (P p1) -- This is wrong ...

-- g :: Integer -> StateOp Integer
-- g x = 
    -- def 1 (x + 4) >~> \p ->
    -- get (P p) >~> \y ->
    -- returnVal (x * y)	

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp a -- -> Memory
	
	-- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp a -- -> (Pointer a, Memory)

    (>>>) :: StateOp a -> StateOp b -> StateOp b
	
    (>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
	
    returnVal :: a -> StateOp a
	
--StateOp (mem -> (a, newMem))
-- Current Issues:
-- set and def are returning StateOps instead of correct value
instance Mutable Integer where
    get (P x) = StateOp (\mem -> 
                        if (inList mem x)
                            then (getInt (lookupA mem x), mem) 
                            else error "Invalid Memory Address")
        

    set (P x) newVal = let s = 
				StateOp (\mem ->
					if (inList mem x)
						then (x, updateA mem (x, (IntVal newVal)))
						else error "Invalid Memory Address")
				in s

							
    def key val = let s =
					StateOp (\mem ->
						if (inList mem key)
						    then error "Memory Already in Use"
                            else (key, (insertA mem (key, (IntVal val)))))
                   in s
    
    op1 >>> op2 = StateOp (\mem1 ->
							let (_, mem2) = runOp op1 mem1
							in runOp op2 mem2)

    op1 >~> op2 = StateOp (\mem1 ->
							let (x, mem2) = runOp op1 mem1
							    newOp = op2 x 
							in runOp newOp mem2)
							
    returnVal x = StateOp (\mem ->
							(x, mem))
	
	
instance Mutable Bool where
    get (P x) = StateOp (\mem -> 
                        if (inList mem x)
                            then (getBool (lookupA mem x), mem) 
                            else error "Invalid Memory Address")
        

    set (P x) newVal = let s = 
				StateOp (\mem ->
					if (inList mem x)
						then (True, updateA mem (x, (BoolVal newVal)))
						else error "Invalid Memory Address")
				in s

							
    def key val = let s =
					StateOp (\mem ->
						if (inList mem key)
						    then error "Memory Already in Use"
                            else (True, (insertA mem (key, (BoolVal val)))))
                   in s
    
    op1 >>> op2 = StateOp (\mem1 ->
							let (_, mem2) = runOp op1 mem1
							in runOp op2 mem2)

    op1 >~> op2 = StateOp (\mem1 ->
							let (x, mem2) = runOp op1 mem1
							    newOp = op2 x 
							in runOp newOp mem2)
							
    returnVal x = StateOp (\mem ->
							(x, mem))	
	
	
	
	
-- Code For Original Values (Not StateOps)                      
-- instance Mutable Integer where
    -- get mem (P x) =  if (inList mem x)
                        -- then getInt (lookupA mem x)
                        -- else error "Invalid Memory Address"

    -- set mem (P x) newVal = if (inList mem x)
                        -- then updateA mem (x, (IntVal newVal))
                        -- else error "Invalid Memory Address"
    -- -- TODO: make sure valid input ?
    -- def mem key val = if (inList mem key)
                        -- then error "Memory Already in Use"
                        -- else ((P key), (insertA mem (key, (IntVal val))))

-- instance Mutable Bool where
    -- get mem (P x) =  if (inList mem x)
                        -- then getBool (lookupA mem x)
                        -- else error "Invalid Memory Address"

    -- set mem (P x) newVal = if (inList mem x)
                        -- then updateA mem (x, (BoolVal newVal))
                        -- else error "Invalid Memory Address"
    
    -- def mem key val = if (inList mem key)
                        -- then error "Memory Already in Use"
                        -- else ((P key), (insertA mem (key, (BoolVal val))))
                        
