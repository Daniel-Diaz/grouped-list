
{-# LANGUAGE TupleSections #-}

module Data.GroupedList
  ( -- * Type
    Grouped
    -- * Builders
  , point
  , concat
  , concatMap
  , replicate
  , fromGroup
    -- * Indexing
  , index
  , adjust
  , adjust2
    -- * Mapping
  , map
  , map2
    -- * Filtering
  , partition
  , filter
    -- * Sorting
  , sort
    -- * Groups
  , Group
  , buildGroup
  , groupElement
  , groupedGroups
    ) where

import Prelude hiding
  (concat, concatMap, replicate, filter, map)
import qualified Prelude as Prelude
import Data.Pointed
import Data.Foldable (toList)
import Data.List (group, foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Control.Arrow ((***))
import Data.Monoid ((<>))

------------------------------------------------------------------
------------------------------------------------------------------
-- GROUP

-- | A 'Group' is a non-empty finite list that contains the same element
--   repeated a number of times.
data Group a = Group {-# UNPACK #-} !Int a deriving Eq

-- | Build a group by repeating the given element a number of times.
--   If the given number is less or equal to 0, 'Nothing' is returned.
buildGroup :: Int -> a -> Maybe (Group a)
buildGroup n x = if n <= 0 then Nothing else Just (Group n x)

-- | Get the element of a group.
groupElement :: Group a -> a
groupElement (Group _ a) = a

-- | A group is larger than other if its constituent element is
--   larger. If they are equal, the group with more elements is
--   the larger.
instance Ord a => Ord (Group a) where
  Group n a <= Group m b =
    if a == b
       then n <= m
       else a <  b

instance Pointed Group where
  point = Group 1

instance Functor Group where
  fmap f (Group n a) = Group n (f a)

instance Foldable Group where
  foldMap f (Group n a) = mconcat $ Prelude.replicate n $ f a
  elem x (Group _ a) = x == a
  null _ = False
  length (Group n _) = n

instance Show a => Show (Group a) where
  show = show . toList

groupJoin :: Group (Group a) -> Group a
groupJoin (Group n (Group m a)) = Group (n*m) a

groupBind :: Group a -> (a -> Group b) -> Group b
groupBind gx f = groupJoin $ fmap f gx

instance Applicative Group where
  pure = point
  gf <*> gx = groupBind gx $ \x -> fmap ($x) gf

instance Monad Group where
  return = pure
  (>>=) = groupBind

------------------------------------------------------------------
------------------------------------------------------------------
-- GROUPED

-- | Type of grouped lists. Grouped lists are finite lists that
--   behave well in the abundance of sublists that have all their
--   elements equal.
newtype Grouped a = Grouped (Seq (Group a)) deriving Eq

-- | Build a grouped list from a regular list. It doesn't work if
--   the input list is infinite.
fromList :: Eq a => [a] -> Grouped a
fromList = Grouped . S.fromList . fmap (\g -> Group (length g) $ head g) . group

-- | Build a grouped list from a group (see 'Group').
fromGroup :: Group a -> Grouped a
fromGroup = Grouped . point

groupedGroups :: Grouped a -> [Group a]
groupedGroups (Grouped gs) = toList gs

instance Pointed Grouped where
  point = fromGroup . point

instance Eq a => Monoid (Grouped a) where
  mempty = Grouped S.empty
  mappend (Grouped gs) (Grouped gs') = Grouped $
    case S.viewr gs of
      gsl S.:> Group n l ->
        case S.viewl gs' of
          Group m r S.:< gsr ->
            if l == r
               then gsl S.>< (Group (n+m) l S.<| gsr)
               else gs S.>< gs'
          _ -> gs
      _ -> gs'

map :: Eq b => (a -> b) -> Grouped a -> Grouped b
map f (Grouped gs) = Grouped $
  case S.viewl gs of
    g S.:< xs ->
      let go (acc, Group n a') (Group m b) =
             let b' = f b
             in  if a' == b'
                    then (acc, Group (n + m) a')
                    else (acc S.|> Group n a', Group m b')
      in  (uncurry (S.|>)) $ foldl go (S.empty, fmap f g) xs
    _ -> S.empty

map2 :: Eq b => (a -> b) -> Grouped a -> Grouped b
map2 f (Grouped gs) = foldr go mempty gs
  where
    go g r = fromGroup (fmap f g) <> r

instance Foldable Grouped where
  foldMap f (Grouped gs) = foldMap (foldMap f) gs
  length (Grouped gs) = foldl' (+) 0 $ fmap length gs
  null (Grouped gs) = null gs

instance Show a => Show (Grouped a) where
  show = show . toList

------------------------------------------------------------------
------------------------------------------------------------------
-- Monad instance (almost)

-- | Concatenation of grouped lists.
concat :: Eq a => Grouped (Grouped a) -> Grouped a
concat = foldMap id

-- | Map a function that produces a grouped list for each element
--   in a grouped list, then concat the results.
concatMap :: Eq b => Grouped a -> (a -> Grouped b) -> Grouped b
concatMap gx f = concat $ map f gx

------------------------------------------------------------------
------------------------------------------------------------------
-- Builders

-- | Replicate a single element the given number of times.
--   If the given number is less or equal to zero, it produces
--   an empty list.
replicate :: Int -> a -> Grouped a
replicate n x = Grouped $
  if n <= 0
     then mempty
     else S.singleton $ Group n x

------------------------------------------------------------------
------------------------------------------------------------------
-- Sorting

-- | Sort a grouped list.
sort :: Ord a => Grouped a -> Grouped a
sort (Grouped xs) = Grouped $ S.unstableSort xs

------------------------------------------------------------------
------------------------------------------------------------------
-- Filtering

-- | Break a grouped list in the elements that match a given condition
--   and those that don't.
partition :: Eq a => (a -> Bool) -> Grouped a -> (Grouped a, Grouped a)
partition f (Grouped xs) = foldr go (mempty, mempty) xs
  where
    go g (gtrue,gfalse) =
      if f $ groupElement g
         then (fromGroup g <> gtrue,gfalse)
         else (gtrue,fromGroup g <> gfalse)

-- | Filter a grouped list by keeping only those that match a given condition.
filter :: Eq a => (a -> Bool) -> Grouped a -> Grouped a
filter f = fst . partition f

------------------------------------------------------------------
------------------------------------------------------------------
-- Indexing

-- | Retrieve the element at the given index. If the index is
--   out of the list index range, it returns 'Nothing'.
index :: Grouped a -> Int -> Maybe a
index (Grouped gs) k = if k < 0 then Nothing else go 0 $ toList gs
  where
    go i (Group n a : xs) =
       let i' = i + n
       in  if k < i'
              then Just a
              else go i' xs
    go _ [] = Nothing

-- expansion/contraction version
adjust :: Eq a => (a -> a) -> Int -> Grouped a -> Grouped a
adjust f k = foldMap point . S.adjust f k . S.fromList . toList

-- list version
adjust2 :: Eq a => (a -> a) -> Int -> Grouped a -> Grouped a
adjust2 f k g@(Grouped gs) = if k < 0 then g else Grouped $ S.fromList $ go k $ toList gs
  where
    -- Pre-condition: 0 <= i
    go i (Group n a : xs)
        -- This condition implies the change only affects current group.
        -- Furthermore:
        --
        --   i <  n - 1  ==>  i + 1 < n
        --   0 <= i      ==>  1 <= i + 1 < n  ==>  n > 1
        --
        --   Therefore, in this case we know n > 1.
        --
      | i < n - 1 =
          let a' = f a
          in  if a == a'
                 then Group n a : xs
                 else if i == 0
                         then Group 1 a' : Group (n-1) a : xs
                              -- Note: i + 1 < n  ==>  0 < n - (i+1)
                         else Group i a : Group 1 a' : Group (n - (i+1)) a : xs
        -- This condition implies the change affects the current group, and can
        -- potentially affect the next group.
      | i == n - 1 =
          let a' = f a
          in  if a == a'
                 then Group n a : xs
                 else if n == 1
                         then case xs of
                                Group m b : ys ->
                                  if a' == b
                                     then Group (m+1) b : ys
                                     else Group 1 a' : xs
                                _ -> [ Group 1 a' ]
                         -- In this branch, n > 1
                         else case xs of
                                Group m b : ys ->
                                  if a' == b
                                     then Group (n-1) a : Group (m+1) b : ys
                                     else Group (n-1) a : Group 1 a' : xs
                                _ -> [ Group (n-1) a , Group 1 a' ]
        -- This condition implies the change affects the next group, and can
        -- potentially affect the current group and the next to the next group.
      | i == n =
          case xs of
            Group m b : ys ->
              let b' = f b
              in  if b == b'
                     then Group n a : xs
                     else if m == 1
                             then if a == b'
                                     then case ys of
                                            Group l c : zs ->
                                              if a == c
                                                 then Group (n+1+l) a : zs
                                                 else Group (n+1) a : ys
                                            _ -> [ Group (n+1) a ]
                                     else Group n a :
                                            case ys of
                                              Group l c : zs ->
                                                if b' == c
                                                   then Group (l+1) c : zs
                                                   else Group 1 b' : ys
                                              _ -> [ Group 1 b' ]
                             -- In this branch, m > 1
                             else if a == b'
                                     then Group (n+1) a : Group (m-1) b : ys
                                     else Group n a : Group 1 b' : Group (m-1) b : ys
            _ -> [ Group n a ]
        -- Otherwise, the current group isn't affected at all.
        -- Note: n < i  ==>  0 < i - n
      | otherwise = go (i-n) xs
    go _ [] = []

------------------------------------------------------------------
------------------------------------------------------------------

traverseGrouped
    :: (Applicative f, Eq b)
    => (Int -> Group a -> f (Grouped b)) -- ^ Function to apply (with group starting index)
    -> Grouped a -- ^ Grouped list
    -> f (Grouped b)
traverseGrouped f (Grouped gs) = snd $ foldl f' (0, pure mempty) gs
  where
    f' (i, fgd) g = (i+length g,) $ mappend <$> fgd <*> f i g
