module Main where

import Data.Numbers.Primes
import Test.Hspec
import Text.Printf

main :: IO ()
main = hspec $ do
  let shouldGiveWithRolled roll scores expected
        = it (take 70 (show scores) ++ "... gives " ++ printf "%5d" expected ++ " when the dice shows " ++ show roll)
        $ calculateScore roll scores `shouldBe` expected
      shouldGiveWith6   = shouldGiveWithRolled 6
      shouldGiveWith5   = shouldGiveWithRolled 5
      shouldGive        = shouldGiveWith6

  describe "basic requirements: adds the scores up" $ do
    take 100 ([0,2] ++ cycle [0,1]) `shouldGive` 51
    take 100 (cycle [0,-1])         `shouldGive` (-50)
    (3:[1..99])                     `shouldGive` 4953
    take 100 ([0,2,0,-1] ++ cycle [0,1,0,-1]) `shouldGive` 1

  describe "requirement 1: add one if the total is positive and even" $ do
    replicate 100 1         `shouldGive` (-395)
    (0 : replicate 99 1)    `shouldGive` (-391)
    replicate 100 0         `shouldGive` (-495)
    replicate 100 (-1)      `shouldGive` (-595)
    (0 : replicate 99 (-1)) `shouldGive` (-589)

    take 100 (cycle [0,1])          `shouldGive` 51
    take 100 (cycle [0,-1])         `shouldGive` (-50)
    take 100 ([2,3] ++ cycle [0,1]) `shouldGive` 55

  describe "requirement 2: 20 bonus points if the result is prime" $ do
    take 100 ([1,0,1,-1] ++ cycle [0,1,0,-1]) `shouldGive` 1
    take 100 ([1,0,2,-1] ++ cycle [0,1,0,-1]) `shouldGive` 23
    take 100 ([1,0,3,-1] ++ cycle [0,1,0,-1]) `shouldGive` 23
    take 100 ([1,0,5,-1] ++ cycle [0,1,0,-1]) `shouldGive` 25
    take 100 ([1,0,7,-1] ++ cycle [0,1,0,-1]) `shouldGive` 27
    take 100 ([1,0,9,-1] ++ cycle [0,1,0,-1]) `shouldGive` 9

    take 100 ([1,0,-7,-1] ++ cycle [0,1,0,-1]) `shouldGive` (-7)

  describe "requirement 3: five point penalty for repeated scores" $ do
    take 100 (0         :  cycle [0,1]) `shouldGive` 45
    take 100 ([0,1]     ++ cycle [1,0]) `shouldGive` 45
    take 100 ([0,7,7,7] ++ cycle [0,1]) `shouldGive` 79
    take 100 ([0,1,1,1] ++ cycle [1,0]) `shouldGive` 57

    ([0,1,2,2]         ++ [4..99]) `shouldGive` 4945
    ([0,1,2,2,2]       ++ [5..99]) `shouldGive` 4957
    ([0,1,2,2,4,5,6,6] ++ [8..99]) `shouldGive` 4939
    replicate 100 5                `shouldGive` 25
    replicate 100 10               `shouldGive` 505

  describe "requirement 9: 100 bonus points if the last digit of the score matches a dice roll" $ do
    take 100 ([0, 55] ++ cycle [1,-1]) `shouldGiveWith6`   55
    take 100 ([0, 55] ++ cycle [1,-1]) `shouldGiveWith5`  155
    take 100 ([0,-55] ++ cycle [1,-1]) `shouldGiveWith6` (-55)
    take 100 ([0,-55] ++ cycle [1,-1]) `shouldGiveWith5`   45





















































calculateScore :: Integer -> [Integer] -> Integer
calculateScore roll = req9BonusIfLastDigitMatchesRoll . req2BonusIfPrime . req1BonusIfPositiveAndEven . go
  where
  req9BonusIfLastDigitMatchesRoll = bonus 100 (\x -> mod x 10 == roll)
  req2BonusIfPrime                = bonus 20  (\x -> x > 0 && isPrime x)
  req1BonusIfPositiveAndEven      = bonus 1   (\x -> x > 0 && even x)
 
  bonus points condition x = if condition x then points + x else x

  go [] = 0
  go [x] = x
  go (x1:xs@(x2:_)) | x1 == x2  = x1 - 5 + go xs
                    | otherwise = x1     + go xs

