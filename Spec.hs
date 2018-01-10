module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  let shouldGive scores expected = it (take 70 (show scores) ++ "... gives " ++ show expected) $ calculateScore scores `shouldBe` expected

  describe "basic requirements: adds the scores up" $ do
    take 100 ([0,2] ++ cycle [0,1]) `shouldGive` 51
    take 100 (cycle [0,-1])         `shouldGive` (-50)
    (3:[1..99])                     `shouldGive` 4953

  describe "requirement 1: add one if the total is positive and even" $ do
    replicate 100 1         `shouldGive` (-395)
    (0 : replicate 99 1)    `shouldGive` (-391)
    replicate 100 0         `shouldGive` (-495)
    replicate 100 (-1)      `shouldGive` (-595)
    (0 : replicate 99 (-1)) `shouldGive` (-589)

    take 100 (cycle [0,1])          `shouldGive` 51
    take 100 (cycle [0,-1])         `shouldGive` (-50)
    take 100 ([1,2] ++ cycle [0,1]) `shouldGive` 53

  describe "requirement 3: five point penalty for repeated scores" $ do
    take 100 (0         :  cycle [0,1]) `shouldGive` 45
    take 100 ([0,1]     ++ cycle [1,0]) `shouldGive` 45
    take 100 ([0,1,1,1] ++ cycle [0,1]) `shouldGive` 41
    take 100 ([0,1,1,1] ++ cycle [1,0]) `shouldGive` 37

    ([0,1,2,2]         ++ [4..99]) `shouldGive` 4945
    ([0,1,2,2,2]       ++ [5..99]) `shouldGive` 4937
    ([0,1,2,2,4,5,6,6] ++ [8..99]) `shouldGive` 4939
    replicate 100 5                `shouldGive` 5
    replicate 100 10               `shouldGive` 505























































calculateScore :: [Integer] -> Integer
calculateScore = incrementIfPositiveAndEven . go
  where
  incrementIfPositiveAndEven x = if even x && x > 0 then x + 1 else x

  go [] = 0
  go [x] = x
  go (x1:xs@(x2:_)) | x1 == x2  = x1 - 5 + go xs
                    | otherwise = x1     + go xs

