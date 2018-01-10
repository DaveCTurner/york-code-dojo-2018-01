module Main where

import Data.Numbers.Primes
import Test.Hspec

main :: IO ()
main = hspec $ do
  let shouldGive scores expected = it (take 70 (show scores) ++ "... gives " ++ show expected) $ calculateScore scores `shouldBe` expected

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

  describe "requirement 2: add twenty if the result is prime" $ do
    take 100 ([1,0,1,-1] ++ cycle [0,1,0,-1]) `shouldGive` 1
    take 100 ([1,0,2,-1] ++ cycle [0,1,0,-1]) `shouldGive` 23
    take 100 ([1,0,3,-1] ++ cycle [0,1,0,-1]) `shouldGive` 23
    take 100 ([1,0,5,-1] ++ cycle [0,1,0,-1]) `shouldGive` 25
    take 100 ([1,0,7,-1] ++ cycle [0,1,0,-1]) `shouldGive` 27
    take 100 ([1,0,9,-1] ++ cycle [0,1,0,-1]) `shouldGive` 9

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























































calculateScore :: [Integer] -> Integer
calculateScore = addTwentyIfPrime . incrementIfPositiveAndEven . go
  where
  addTwentyIfPrime x = if x > 0 && isPrime x then x + 20 else x
  incrementIfPositiveAndEven x = if even x && x > 0 then x + 1 else x

  go [] = 0
  go [x] = x
  go (x1:xs@(x2:_)) | x1 == x2  = x1 - 5 + go xs
                    | otherwise = x1     + go xs

