
{-# LANGUAGE TupleSections, TypeFamilies, CPP #-}

-- | Grouped lists are like lists, but internally they are represented
--   as groups of consecutive elements.
--
--   For example, the list @[1,2,2,3,4,5,5,5]@ would be internally
--   represented as @[[1],[2,2],[3],[4],[5,5,5]]@. Use 'groupedGroups'
--   to see this.
--
module Data.GroupedList
  ( -- * Type
    Grouped
    -- * Builders
  , empty
  , point
    -- | Use 'point' to create a 'Grouped' list with a single element.
  , concatMap
  , replicate
  , fromGroup
    -- * Info
  , length
    -- * Indexing
  , index
  , adjust
  , adjustM
    -- * Sublists
  , take
  , drop
    -- * Mapping
  , map
    -- * Traversal
  , traverseGrouped
  , traverseGroupedByGroup
  , traverseGroupedByGroupAccum
    -- * Filtering
  , partition
  , filter
    -- * Sorting
  , sort
    -- * Zipping
  , zipWith
    -- * List conversion
    -- | For to-list conversion use 'toList'.
  , fromList
    -- * Groups
  , Group
  , buildGroup
  , groupElement
  , groupSize
    -- ** In grouped lists
  , groupedGroups
  , firstGroup
  , lastGroup
    ) where

import Prelude hiding
  ( concat, concatMap, replicate, filter, map
  , take, drop, foldl, foldr, length, zipWith
    )
import qualified Prelude as Prelude
import Data.Pointed
import Data.Foldable (Foldable (..), toList, foldrM)
import Data.List (group)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Monoid ((<>))
import Control.DeepSeq (NFData (..))
import Control.Arrow (second)
import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity (..))

------------------------------------------------------------------
------------------------------------------------------------------
-- COMPATIBILITY

#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts as GHC
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
import Data.Traversable (traverse)
import Data.Monoid (Monoid (..))
#endif

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

-- | Size of a group.
groupSize :: Group a -> Int
groupSize (Group n _) = n

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
#if MIN_VERSION_base(4,8,0)
  elem x (Group _ a) = x == a
  null _ = False
  length (Group n _) = n
#endif

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
#if !MIN_VERSION_base(4,8,0)
  return = pure
#endif
  (>>=) = groupBind

instance NFData a => NFData (Group a) where
  rnf (Group _ a) = rnf a

------------------------------------------------------------------
------------------------------------------------------------------
-- GROUPED

-- | Type of grouped lists. Grouped lists are finite lists that
--   behave well in the abundance of sublists that have all their
--   elements equal.
newtype Grouped a = Grouped (Seq (Group a)) deriving Eq

-- | Grouped list with no elements.
empty :: Grouped a
empty = Grouped S.empty

#if MIN_VERSION_base(4,7,0)
-- | Method 'fromList' doesn't work for infinite lists.
--   A grouped list cannot be infinite.
instance Eq a => GHC.IsList (Grouped a) where
  type (Item (Grouped a)) = a
  fromList = fromList
  toList = toList
#endif

-- | Build a grouped list from a regular list. It doesn't work if
--   the input list is infinite.
fromList :: Eq a => [a] -> Grouped a
fromList = Grouped . S.fromList . fmap (\g -> Group (Prelude.length g) $ head g) . group

-- | Build a grouped list from a group (see 'Group').
fromGroup :: Group a -> Grouped a
fromGroup = Grouped . point

-- | Groups of consecutive elements in a grouped list.
groupedGroups :: Grouped a -> [Group a]
groupedGroups (Grouped gs) = toList gs

-- | Get the first group (if the list is not empty) and
--   the rest of the list.
firstGroup :: Grouped a -> Maybe (Group a, Grouped a)
firstGroup (Grouped gs) =
  case S.viewl gs of
    g S.:< hs -> Just (g, Grouped hs)
    _ -> Nothing

-- | Get the last group (if the list is not empty) and
--   the rest of the list.
lastGroup :: Grouped a -> Maybe (Grouped a, Group a)
lastGroup (Grouped gs) =
  case S.viewr gs of
    hs S.:> g -> Just (Grouped hs,g)
    _ -> Nothing

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

-- | Apply a function to every element in a grouped list.
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

instance Foldable Grouped where
  foldMap f (Grouped gs) = foldMap (foldMap f) gs
#if MIN_VERSION_base(4,8,0)
  length (Grouped gs) = foldl' (+) 0 $ fmap length gs
  null (Grouped gs) = null gs
#else

length :: Grouped a -> Int
length (Grouped gs) = foldl' (+) 0 $ fmap groupSize gs
#endif

instance Show a => Show (Grouped a) where
  show = show . toList

instance NFData a => NFData (Grouped a) where
  rnf (Grouped gs) = rnf gs

------------------------------------------------------------------
------------------------------------------------------------------
-- Monad instance (almost)

-- | Map a function that produces a grouped list for each element
--   in a grouped list, then concat the results.
concatMap :: Eq b => Grouped a -> (a -> Grouped b) -> Grouped b
concatMap gx f = fold $ map f gx

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
sort (Grouped xs) = Grouped $ S.fromList $ fmap (uncurry $ flip Group)
                            $ M.toAscList $ foldr go M.empty xs
  where
    f n (Just k) = Just $ k+n
    f n _ = Just n
    go (Group n a) = M.alter (f n) a

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

-- | Filter a grouped list by keeping only those elements that match a given condition.
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

-- | Update the element at the given index. If the index is out of range,
--   the original list is returned.
adjust :: Eq a => (a -> a) -> Int -> Grouped a -> Grouped a
adjust f i g = runIdentity $ adjustM (Identity . f) i g

-- | Just like 'adjust', but the function returns in a 'Monad'.
#if MIN_VERSION_base(4,8,0)
adjustM :: (Monad m, Eq a) => (a -> m a) -> Int -> Grouped a -> m (Grouped a)
#else
adjustM :: (Applicative m, Monad m, Eq a) => (a -> m a) -> Int -> Grouped a -> m (Grouped a)
#endif
adjustM f k g@(Grouped gs) = if k < 0 then pure g else Grouped <$> go 0 k gs
  where
    -- Pre-condition: 0 <= i
    go npre i gseq =
      case S.viewl gseq of
        Group n a S.:< xs ->
          let pre = S.take npre gs
          in  case () of
                -- This condition implies the change only affects current group.
                -- Furthermore:
                --
                --   i <  n - 1  ==>  i + 1 < n
                --   0 <= i      ==>  1 <= i + 1 < n  ==>  n > 1
                --
                --   Therefore, in this case we know n > 1.
                --
            _ | i < n - 1 -> fmap (pre S.><) $ do
                  a' <- f a
                  pure $
                    if a == a'
                       then gseq
                       else if i == 0
                               then Group 1 a' S.<| Group (n-1) a S.<| xs
                                    -- Note: i + 1 < n  ==>  0 < n - (i+1)
                               else Group i a S.<| Group 1 a' S.<| Group (n - (i+1)) a S.<| xs
                -- This condition implies the change affects the current group, and can
                -- potentially affect the next group.
            _ | i == n - 1 -> fmap (pre S.><) $ do
                  a' <- f a
                  pure $
                    if a == a'
                       then gseq
                       else if n == 1
                               then case S.viewl xs of
                                      Group m b S.:< ys ->
                                        if a' == b
                                           then Group (m+1) b S.<| ys
                                           else Group 1 a' S.<| xs
                                      _ -> S.singleton $ Group 1 a'
                               -- In this branch, n > 1
                               else case S.viewl xs of
                                      Group m b S.:< ys ->
                                        if a' == b
                                           then Group (n-1) a S.<| Group (m+1) b S.<| ys
                                           else Group (n-1) a S.<| Group 1 a' S.<| xs
                                      _ -> S.fromList [ Group (n-1) a , Group 1 a' ]
                -- This condition implies the change affects the next group, and can
                -- potentially affect the current group and the next to the next group.
            _ | i == n -> fmap (pre S.><) $
                  case S.viewl xs of
                    Group m b S.:< ys -> do
                      b' <- f b
                      pure $
                        if b == b'
                           then gseq
                           else if m == 1
                                   then if a == b'
                                           then case S.viewl ys of
                                                  Group l c S.:< zs ->
                                                    if a == c
                                                       then Group (n+1+l) a S.<| zs
                                                       else Group (n+1) a S.<| ys
                                                  _ -> S.singleton $ Group (n+1) a
                                           else Group n a S.<|
                                                  case S.viewl ys of
                                                    Group l c S.:< zs ->
                                                      if b' == c
                                                         then Group (l+1) c S.<| zs
                                                         else Group 1 b' S.<| ys
                                                    _ -> S.singleton $ Group 1 b'
                                   -- In this branch, m > 1
                                   else if a == b'
                                           then Group (n+1) a S.<| Group (m-1) b S.<| ys
                                           else Group n a S.<| Group 1 b' S.<| Group (m-1) b S.<| ys
                    _ -> pure $ S.singleton $ Group n a
                -- Otherwise, the current group isn't affected at all.
                -- Note: n < i  ==>  0 < i - n
            _ | otherwise -> go (npre+1) (i-n) xs
        _ -> pure S.empty

------------------------------------------------------------------
------------------------------------------------------------------
-- Sublists

-- | Take the given number of elements from the left end of the
--   list.
take :: Int -> Grouped a -> Grouped a
take n (Grouped gs) = Grouped $ if n <= 0 then S.empty else go 0 n gs
  where
    go npre k xs =
      case S.viewl xs of
        Group q x S.:< ys ->
          if k <= q
             then S.take npre gs S.|> Group k x
             else go (npre+1) (k-q) ys -- k - q > 0
        _ -> gs

-- | Discard the given number of elements from the left end of
--   the list.
drop :: Int -> Grouped a -> Grouped a
drop n g@(Grouped gs) = if n <= 0 then g else Grouped $ go n gs
  where
    go k xs =
      case S.viewl xs of
        Group q x S.:< ys ->
          if k < q
             then Group (q - k) x S.<| ys
             else go (k - q) ys
        _ -> S.empty

------------------------------------------------------------------
------------------------------------------------------------------
-- Traversal

-- | Apply a function with results residing in an applicative functor to every
--   element in a grouped list.
traverseGrouped :: (Applicative f, Eq b) => (a -> f b) -> Grouped a -> f (Grouped b)
traverseGrouped f = foldr (\x fxs -> mappend <$> (point <$> f x) <*> fxs) (pure mempty)

-- | Similar to 'traverseGrouped', but instead of applying a function to every element
--   of the list, it is applied to groups of consecutive elements. You might return more
--   than one element, so the result is of type 'Grouped'. The results are then concatenated
--   into a single value, embedded in the applicative functor.
traverseGroupedByGroup :: (Applicative f, Eq b) => (Group a -> f (Grouped b)) -> Grouped a -> f (Grouped b)
traverseGroupedByGroup f (Grouped gs) = fold <$> traverse f gs

-- | Like 'traverseGroupedByGroup', but carrying an accumulator.
--   Note the 'Monad' constraint instead of 'Applicative'.
traverseGroupedByGroupAccum ::
#if MIN_VERSION_base(4,8,0)
  (Monad m, Eq b)
#else
  (Applicative m, Monad m, Eq b)
#endif
   => (acc -> Group a -> m (acc, Grouped b))
   -> acc -- ^ Initial value of the accumulator.
   -> Grouped a
   -> m (acc, Grouped b)
traverseGroupedByGroupAccum f acc0 (Grouped gs) = foldrM go (acc0, mempty) gs
  where
    go g (acc, gd) = second (<> gd) <$> f acc g

------------------------------------------------------------------
------------------------------------------------------------------
-- Zipping

zipWith :: Eq c => (a -> b -> c) -> Grouped a -> Grouped b -> Grouped c
zipWith f (Grouped xs) (Grouped ys) = fold $ fmap fromGroup $ go xs ys
  where
    go gs gs' =
      case S.viewl gs of
        Group n x S.:< hs ->
          case S.viewl gs' of
            Group m y S.:< hs' ->
              let z = f x y
              in  case () of
                    _ | n == m -> Group n z : go hs hs'
                    _ | n  > m -> Group m z : go (Group (n-m) x S.<| hs) hs'
                    _ -> Group n z : go hs (Group (m-n) y S.<| hs')
            _ -> []
        _ -> []
