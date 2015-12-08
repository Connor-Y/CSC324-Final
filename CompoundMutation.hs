{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
	(>>>), (>~>), returnVal, runOp,
    Memory, Pointer, StateOp,
	-- Testing Exports
	inList, getInt, getBool,
	testMem, p1, p2, p3, p4, testMem2, p21, p22, p23, p24
    )
    where

import AList (AList, lookupA, insertA, updateA, removeA) 
-- A type representing the possible values stored in memory.
data Value = IntVal Integer  |
             BoolVal Bool   
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value
-- Same as [(Integer, StateOp)]

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

data StateOp a = StateOp (Memory -> (a, Memory))

-- Runs a StateOp with a given Memory
runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- Determines if a given key is in the AList
inList :: Eq a => AList a b -> a -> Bool
inList alist key =  if (null alist)
                        then False
                        else 
                            if (fst (head alist)) == key
                                then True
                                else inList (tail alist) key    
            
-- Finds the maximum key used in an AList
maxKey :: Ord a => AList a b -> a -> a
maxKey alist max = if (null alist)
						then max
						else 
						    if (fst (head alist)) > max
                                then maxKey (tail alist) (fst (head alist))
                                else maxKey (tail alist) max

-- Converts an IntVal into an Integer								
getInt :: Value -> Integer
getInt (IntVal x) = x
getInt _ = error "Invalid Type"

-- Converts a BoolVal into a Bool
getBool :: Value -> Bool
getBool (BoolVal x) = x
getBool _ = error "Invalid Type"

p1 :: Pointer Integer
p1 = (P 1)
p2 :: Pointer Integer
p2 = (P 2)

p3 :: Pointer Bool
p3 = (P 3)

p4 :: Pointer Bool
p4 = (P 4)

p21 :: Pointer Integer
p21 = (P 1)

p22 :: Pointer Integer
p22 = (P 2)

p23 :: Pointer Integer
p23 = (P 3)

p24 :: Pointer Integer
p24 = (P 4)
testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]
testBool = [(3, BoolVal True), (4, BoolVal False)]
testMem2 = [(1, IntVal 10), (2, IntVal 30), (3, IntVal 50), (4, IntVal 70)]




-- f :: StateOp Integer
-- f = def 1 (4 :: Integer) >~> \p -> get p

-- g :: Integer -> StateOp Integer
-- g x = 
    -- def 1 (x + 4) >~> \p ->
    -- get (P p) >~> \y ->
    -- returnVal (x * y)	

	
-- h x = def x x >~> \p1 -> get p1
	
-- i = get p2 >~> \v1 -> set p1 v1 >>> set p2 0

-- f :: (Integer, Memory)
-- f = runOp (def 1 4 >~> \p1 -> get p1) []

-- Junk Code
-- formatSetResult op mem = let result = runOp op mem
                         -- in snd result
						 
-- formatDefResult op mem = let result = runOp op mem
                         -- in ((P (fst result)), snd result)


						 
-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Mutable a => Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Mutable a => Pointer a -> a -> StateOp () 
	
	-- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Mutable a =>  Integer -> a -> StateOp (Pointer a) 

	-- Then Operation. Perform the first StateOp returning only the new Memory.
	-- Perform the second StateOp on the new Memory.
    (>>>) :: StateOp a -> StateOp b -> StateOp b
	
	-- Bind Operation. Perform the first StateOp returning the resultant value 
	-- and Memory. Create a second StateOp using the value from the first one.
    -- Run the second StateOp on the returned Memory.
    (>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
	
	-- Given a value, create a new StateOp that returns that value without
	-- changing the Memory
    returnVal :: a -> StateOp a

	-- Given a value, store it in any available Memory and return a Pointer to it,
	-- as well as, the new Memory
    alloc :: Mutable a => a -> StateOp (Pointer a)
	
	-- Given a Pointer to something in Memory, return a new Memory without the item
	-- indicated by the Pointer.
    free :: Mutable a => Pointer a -> StateOp ()

instance Mutable Integer where
    get (P x) = StateOp (\mem -> 
                        if (inList mem x)
                            then (getInt (lookupA mem x), mem) 
                            else error "Invalid Memory Address")
        

    set (P x) newVal = StateOp (\mem ->
						if (inList mem x)
							then ((), updateA mem (x, (IntVal newVal)))
							else error "Invalid Memory Address")

							
    def key val = StateOp (\mem ->
					if (inList mem key)
						then error "Memory Already in Use"
						else ((P key), (insertA mem (key, (IntVal val)))))

    
    op1 >>> op2 = StateOp (\mem1 ->
							let (_, mem2) = runOp op1 mem1
							in runOp op2 mem2)

    op1 >~> op2 = StateOp (\mem1 ->
							let (x, mem2) = runOp op1 mem1
							    newOp = op2 x 
							in runOp newOp mem2)
							
    returnVal x = StateOp (\mem ->
							(x, mem))
	
    alloc newVal = StateOp (\mem ->
						    let newKey = (maxKey mem 0) + 1
							in ((P newKey) :: Pointer Integer, (insertA mem (newKey, (IntVal newVal)))))
	
    free (P x) = StateOp (\mem ->
						    ((), removeA mem x))
	
instance Mutable Bool where
    get (P x) = StateOp (\mem -> 
                        if (inList mem x)
                            then (getBool (lookupA mem x), mem) 
                            else error "Invalid Memory Address")
        

    set (P x) newVal = 
				StateOp (\mem ->
					if (inList mem x)
						then ((), updateA mem (x, (BoolVal newVal)))
						else error "Invalid Memory Address")
							
    def key val = 
			    StateOp (\mem ->
				    if (inList mem key)
					    then error "Memory Already in Use"
					    else ((P key), (insertA mem (key, (BoolVal val)))))
                  
    
    op1 >>> op2 = StateOp (\mem1 ->
							let (_, mem2) = runOp op1 mem1
							in runOp op2 mem2)

    op1 >~> op2 = StateOp (\mem1 ->
							let (x, mem2) = runOp op1 mem1
							    newOp = op2 x 
							in runOp newOp mem2)
							
    returnVal x = StateOp (\mem ->
							(x, mem))	
	
    alloc newVal = StateOp (\mem ->
					    let newKey = (maxKey mem 0) + 1
					    in ((P newKey) :: Pointer Bool, (insertA mem (newKey, (BoolVal newVal)))))
	
    free (P x) = StateOp (\mem ->
						    ((), removeA mem x))
							
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
                        