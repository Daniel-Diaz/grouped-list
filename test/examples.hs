
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import System.Exit (exitFailure)
import Data.GroupedList (Grouped)
import qualified Data.GroupedList as GL
import Control.Monad (unless)
import Data.Functor.Identity

(=:) :: (Show a, Eq a) => a -> a -> IO ()
x =: y = unless (x == y) $ do
  print x
  putStrLn " /= "
  print y
  exitFailure

main :: IO ()
main = do
  (GL.empty :: Grouped ())
    =: []
  (GL.point 0 :: Grouped Int)
    =: [0]
  GL.replicate 3 0
    =: [0,0,0]
  GL.index [1,2,3] 1
    =: Just 2
  GL.index [1,2,3] 3
    =: Nothing
  GL.adjust (+1) 1 [1,2,2]
    =: [1,3,2]
  GL.adjust (+1) 3 [1,2,3]
    =: [1,2,3]
  GL.take 2 [1,1,1,2]
    =: [1,1]
  GL.take 2 [1,2,2,2]
    =: [1,2]
  GL.take 2 [1,2,3,4]
    =: [1,2]
  GL.drop 2 [1,1,1,2]
    =: [1,2]
  GL.drop 2 [1,2,2,2]
    =: [2,2]
  GL.drop 2 [1,2,3,4]
    =: [3,4] 
  GL.map (+1) [1,2,3,4]
    =: [2,3,4,5]
  GL.map (+1) [1,1,2,3]
    =: [2,2,3,4]
  GL.map (const 0) [1,2,2,3]
    =: [0,0,0,0]
  GL.partition even [1,2,3,4]
    =: ([2,4],[1,3])
  GL.partition even [1,2,1,2]
    =: ([2,2],[1,1])
  GL.sort [1,2,1,2]
    =: [1,1,2,2]
  GL.sort [3,2,1,3]
    =: [1,2,3,3]
  GL.zipWith (+) [1,2,1,1] [2,1,1,2]
    =: [3,3,2,3]
  runIdentity (fmap snd $ GL.traverseGroupedByGroupAccum (\acc g -> Identity (acc+1, GL.map (+acc) $ GL.fromGroup g)) 0 [1,2,3,3,4])
    =: [1,3,5,5,7]
