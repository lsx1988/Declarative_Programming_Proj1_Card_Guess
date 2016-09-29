module Assignment1 (elementPosition, everyNth, elementBefore) where

elementPosition :: Eq t => t -> [t] -> Int
elementPosition elt (l:ls)
 | elt == l = 0
 | otherwise = elementPosition elt ls + 1
elementPosition elt [] = error "No match"

everyNth :: Int -> [t] -> [t]
everyNth n lst 
 | n <= 0 = error "No match"
 | n - 1 >= length lst = []
 | otherwise = [lst !! (n-1)] ++ everyNth n (drop n lst)

elementBefore :: Eq a => a -> [a] -> Maybe a
elementBefore n lst
 | n `notElem` lst = Nothing
 | elementPosition n lst == 0 = Nothing
 | otherwise = Just (lst !! ((elementPosition n lst) - 1))