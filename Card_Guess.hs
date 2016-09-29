module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

type GameState = [[Card]]
cards = [minBound..maxBound] :: [Card]

{--------------------feebback function-----------------------------------------------------------}

{-
 Return 5 Ints based on answer rules, each Int is calculate by one function
-}

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback target guess = (feedback_1, feedback_2, feedback_3, feedback_4, feedback_5)
    where feedback_1  = match target guess
          feedback_2  = lowerThan target guess
          feedback_3  = rankEqual target guess
          feedback_4  = higherThan target guess
          feedback_5  = suitEqual target guess
{-
 Check the number of cards in guess consistent with the cards in target
 Use map function to see if the guess card is element of target card, return the list of Bool
 Filter the Bool list with True, the length of filtered list is the matched card number
 E.g. [Card1, Card2] [Card1, Card3] -> [True, False] -> [True] -> 1
-}

match :: [Card] -> [Card] -> Int
match target guess = length.filter (==True) $ map (\g -> g `elem` target)  guess

{-
 Check the number of cards in the answer whose rank is lower than the lowest rank in the guess
 (map (rank) guess) -> Convert card list to its corresponding rank list
 (map (\t -> rank(t) < minmumRankInGuess) target) -> Convert the target list to Bool list. 
 The number of Ture is the result
-}

lowerThan :: [Card] -> [Card] -> Int
lowerThan target guess = length.filter (==True) $ map (\t -> rank(t) < minmumRankInGuess) target
                      where minmumRankInGuess = minimum $ map (rank) guess
{-
 Check the number of cards in the answer have rank highest than the highest rank in the guess
 Same method as lowest.
-}

higherThan :: [Card] -> [Card] -> Int
higherThan target guess = length.filter (==True) $ map (\t -> rank(t) > maximumRankInGuess) target
                       where maximumRankInGuess = maximum $ map (rank) guess
{-
 Check the number of equal rank between target & guess list
-}

rankEqual :: [Card] -> [Card] -> Int
rankEqual target guess = minimum [rankInAnswer, rankInGuess]
    where rankInAnswer = length.filter (==True) $ map (\t -> rank(t) `elem` rankListOfGuess) target
                          where rankListOfGuess  = map (rank) guess
          rankInGuess  = length.filter (==True) $ map (\g -> rank(g) `elem` rankListOfAnswer) guess
                          where rankListOfAnswer = map (rank) target
{-
 Check the number of equal suit between target & guess list
-}

suitEqual :: [Card] -> [Card] -> Int
suitEqual target guess = minimum [suitInAnswer, suitInGuess]
    where suitInAnswer = length.filter (==True) $ map (\t -> suit(t) `elem` suitListOfGuess) target
                            where suitListOfGuess  = map (suit) guess                                        
          suitInGuess  = length.filter (==True) $ map (\g -> suit(g) `elem` suitListOfAnswer) guess
                            where suitListOfAnswer = map (suit) target

{----------------------initiaGuess function---------------------------------------------------------}

{-
 Given the card number, list out all of its pair, take the first as initial guess, the rest as GameState
-}
initialGuess :: Int -> ([Card], GameState)
initialGuess numOfCards = (head allCardPair, tail allCardPair)
 where allCardPair = buildCardPair numOfCards

{-
 Build up the card pair list, use sequence to get all the possible combination
 first remove the pair that contains only one kind of card, eg. [2C,2C]
 then make the pair unique, eg. [2C,3C] & [3C,2C] -> [2C,3C]
-}
buildCardPair :: Int -> [[Card]]
buildCardPair n      = removeSamePair $ map sort $ filter isCardDuplicateInPair allCardPair
   where allCardPair = sequence (decksOfCard n)

{-
 Combine several decks of cards together based the card number
-}
decksOfCard :: Int -> [[Card]]
decksOfCard 1 = [cards]
decksOfCard n = cards : decksOfCard (n-1)

{-
 Chech if the element in a list is unique
-}
isCardDuplicateInPair :: Eq a => [a] -> Bool
isCardDuplicateInPair [] = True
isCardDuplicateInPair (x:xs)
 | x `elem` xs = False
 | otherwise   = isCardDuplicateInPair xs

{-
 Make the element in a list unique
-}
removeSamePair :: Eq a => [a] -> [a]
removeSamePair (x:xs) = x : removeSamePair (filter (/=x) xs)
removeSamePair [] = []

{----------------------nextGuess function---------------------------------------------------------}

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, cardPairList) (f1,f2,f3,f4,f5) = (newGuess, newCardPairList)
 where  newCardPairList    = lastFilter
          where filter1    = filter (\p -> match p guess      == f1) cardPairList
                filter2    = filter (\p -> lowerThan p guess  == f2) filter1
                filter3    = filter (\p -> rankEqual p guess  == f3) filter2
                filter4    = filter (\p -> higherThan p guess == f4) filter3
                lastFilter = filter (\p -> suitEqual p guess  == f5) filter4                  
        newGuess
         | length guess == 2 = getPossibleCard newCardPairList
         | otherwise         = head newCardPairList
{-
 Choose the next guess from GameState, the pattern is based on Hint3
-}         
getPossibleCard :: [[Card]] -> [Card]
getPossibleCard cs = head [ c | (c,score) <- (zip cs csScore), score == minimum csScore]
     where csScore = possibleCardScore cs
{-
 Calculate the possible cards pair left if current card pair is chosen for next guess
-}
possibleCardScore :: [[Card]] -> [Double]
possibleCardScore cs = map (\c -> (getPossibility $ map (feedback c) cs) ) cs

{-
 Given a list, calculate the ratio of each kind of element
-}
getPossibility :: (Eq a, Ord a) => [a] -> Double
getPossibility xs = calculate $ map (length) $ (group.sort) xs

calculate :: [Int] -> Double
calculate xs = fromIntegral (sum $ map (^2) xs) / fromIntegral (sum xs)