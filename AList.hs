{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
	removeA
    )
    where

-- Association List or Dictionary
type AList a b = [(a, b)]


-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = if (fst (head alist)) == key
						then (snd (head alist))
						else lookupA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) = if (inlist alist key)
							then alist
							else (key, val):alist

-- Returns true iff the key is in the AList 
inlist :: Eq a => AList a b -> a -> Bool
inlist alist key = 	if (null alist)
						then False
						else 
							if (fst (head alist)) == key
								then True
								else inlist (tail alist) key			

						
-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) = if (inlist alist key)
							then [if (fst x) == key then (key, val) else x | x <- alist] 
							else alist


removeA :: Eq a => AList a b -> a -> AList a b
removeA alist key = [x| x <- alist, (fst x) /= key]
	



