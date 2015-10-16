
module Main (main) where

import Data.GroupedList (Grouped)
import qualified Data.GroupedList as GL
import Data.Monoid ((<>))
import Data.Foldable (toList)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck (Arbitrary (..), Testable, choose)

-- | A short alias for 'QC.testProperty'.
prop :: Testable a => TestName -> a -> TestTree
prop = QC.testProperty

newtype G = G (Grouped Int)

instance Show G where
  show (G g) = show g

instance Arbitrary G where
  arbitrary = G . GL.fromList <$> arbitrary

main :: IO ()
main = defaultMain $ testGroup "Properties"
  [ prop "left mempty" $ \(G xs) -> GL.empty <> xs == xs
  , prop "right mempty" $ \(G xs) -> xs <> GL.empty == xs
  , prop "mappend" $ \(G xs) (G ys) (G zs) -> (xs <> ys) <> zs == xs <> (ys <> zs)
  , prop "index" $ \(G xs) ->
      let n = length xs
      in  (\i -> 
             let r = GL.index xs i
             in  if i <= n - 1
                    then r == Just (toList xs !! i)
                    else r == Nothing
            ) <$> choose (0,2*n)
  , prop "split and concat" $ \(G xs) ->
      let n = length xs
      in  (\i -> GL.take i xs <> GL.drop i xs == xs) <$> choose (0,n)
  , prop "map id" $ \(G xs) -> GL.map id xs == xs
  , prop "map (+1) . map (+1) = map (+2)" $
      \(G xs) -> GL.map (+1) (GL.map (+1) xs) == GL.map (+2) xs
  , prop "'map (const i)' generates at most one group" $
      \(G xs) i -> length (GL.groupedGroups $ GL.map (const i) xs :: [GL.Group Int]) <= 1
    ]
