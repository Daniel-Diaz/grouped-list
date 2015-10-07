
module Main (main) where

import Data.GroupedList (Grouped)
import qualified Data.GroupedList as G
import Criterion.Main
  ( defaultMainWith, bgroup, bench, nf
  , Benchmarkable, Benchmark
  , defaultConfig
    )
import Criterion.Types (reportFile)

sampleSize :: Int
sampleSize = 1000

sampleSize2 :: Int
sampleSize2 = div sampleSize 2

uniform :: Grouped Int
{-# NOINLINE uniform #-}
uniform = G.fromList $ replicate sampleSize 0

increasing :: Grouped Int
{-# NOINLINE increasing #-}
increasing = G.fromList $ [1 .. sampleSize]

halfuniform :: Grouped Int
{-# NOINLINE halfuniform #-}
halfuniform = G.fromList $ replicate sampleSize2 0 ++ [1 .. sampleSize2]

halfincreasing :: Grouped Int
{-# NOINLINE halfincreasing #-}
halfincreasing = G.fromList $ [1 .. sampleSize2] ++ replicate sampleSize2 0

interleaved :: Grouped Int
{-# NOINLINE interleaved #-}
interleaved = G.fromList $ concat $ zipWith (\x y -> [x,y]) (replicate sampleSize2 0) [1 .. sampleSize2]

benchGroup :: String -> (Grouped Int -> Benchmarkable) -> Benchmark
benchGroup n f = bgroup n $ fmap (\(bn,xs) -> bench bn $ f xs)
  [ ("uniform", uniform)
  , ("increasing", increasing)
  , ("halfuniform", halfuniform)
  , ("halfincreasing" , halfincreasing)
  , ("interleaved", interleaved)
    ]

main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "grouped-list-bench.html" })
  [ benchGroup "map id" $ nf $ G.map id
  , benchGroup "map +1" $ nf $ G.map (+1)
  , benchGroup "map const" $ nf $ G.map (const True)
  , benchGroup "adjust 0/2" $ nf $ G.adjust (+1) 0
  , benchGroup "adjust 1/2" $ nf $ G.adjust (+1) $ sampleSize2 + 1
  , benchGroup "adjust 2/2" $ nf $ G.adjust (+1) $ sampleSize - 1
  , benchGroup "adjust2 0/2" $ nf $ G.adjust2 (+1) 0
  , benchGroup "adjust2 1/2" $ nf $ G.adjust2 (+1) $ sampleSize2 + 1
  , benchGroup "adjust2 2/2" $ nf $ G.adjust2 (+1) $ sampleSize - 1
    ]
