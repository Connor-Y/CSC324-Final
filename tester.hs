
swap :: 

cycleMap :: [a] -> [a]
cycleMap (x:y:xs) = swap x y : cycleMap (y:xs)		  
cycleMap (x:y) = swap x y
cycleMap x = x
cycleMap _ = error "You Broke It"	