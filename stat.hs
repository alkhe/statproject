module Stat (mean, median, q1, q3, iqr, variance, sdev, zscore, devproduct, strength, lowerFence, upperFence, stripOutliers) where

import Data.Function (on)

type N = Double

mean :: [N] -> N
mean set = sum set / fromIntegral (length set)

median :: [N] -> N
median set = if odd l then mid else (mid + mid') / 2
  where l = length set
        mid = set !! (l `div` 2)
        mid' = set !! (l `div` 2 - 1)

lowerHalf :: [N] -> [N]
lowerHalf set = take elements set
  where l = length set
        elements = if odd l
          then l `div` 2 - 1
          else l `div` 2

upperHalf :: [N] -> [N]
upperHalf set = drop (l - elements) set
  where l = length set
        elements = if odd l
          then l `div` 2 - 1
          else l `div` 2

q1 :: [N] -> N
q1 = median . lowerHalf

q3 :: [N] -> N
q3 = median . upperHalf

iqr :: [N] -> N
iqr = (-) <$> q3 <*> q1

squaresum :: [N] -> N
squaresum set = foldr (\x s -> s + (x - x') ** 2) 0 set
  where x' = mean set

variance :: [N] -> N
variance set = squaresum set / fromIntegral (length set - 1)

sdev :: [N] -> N
sdev = sqrt . variance

zscore :: [N] -> [N]
zscore set = (\x -> (x - x') / s) <$> set
  where s = sdev set
        x' = mean set

devproduct :: [N] -> [N] -> [N]
devproduct = zipWith (*) `on` zscore

strength :: [(N, N)] -> N
strength biset = (sum . uncurry devproduct) (xs, ys) / fromIntegral (n - 1)
  where xs = fst <$> biset
        ys = snd <$> biset
        n = length biset

lowerFence :: [N] -> N
lowerFence = (-) <$> q1 <*> (* 1.5) . iqr

upperFence :: [N] -> N
upperFence = (+) <$> q3 <*> (* 1.5) . iqr

stripOutliers :: [N] -> [N]
stripOutliers set = filter (between <$> lowerFence <*> upperFence $ set) set
  where between a b = (&&) <$> (a <=) <*> (<= b)
