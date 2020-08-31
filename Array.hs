{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}

module Array where

import GHC.Exts (SmallArray#, newSmallArray#, readSmallArray#, writeSmallArray#,
                 indexSmallArray#, unsafeFreezeSmallArray#, unsafeThawSmallArray#,
                 SmallMutableArray#, sizeofSmallArray#, copySmallArray#, thawSmallArray#,
                 sizeofSmallMutableArray#, copySmallMutableArray#, cloneSmallMutableArray#,
                 deleteSmallArray#, insertSmallArray#, updateSmallArray#)
import GHC.Exts(Int(..), Int#, reallyUnsafePtrEquality#, tagToEnum#, unsafeCoerce#, State#)
import GHC.ST (ST(..), runST)
import Prelude hiding (filter, foldMap, foldr, foldl, length, map, read, traverse, all)
import Control.DeepSeq

# define CHECK_BOUNDS(_func_,_len_,_k_)
# define CHECK_OP(_func_,_op_,_lhs_,_rhs_)
# define CHECK_GT(_func_,_lhs_,_rhs_)
# define CHECK_LE(_func_,_lhs_,_rhs_)
# define CHECK_EQ(_func_,_lhs_,_rhs_)

type Array# a = SmallArray# a
type MutableArray# a = SmallMutableArray# a

newArray# :: Int# -> a -> State# d -> (# State# d, SmallMutableArray# d a #)
newArray# = newSmallArray#

unsafeFreezeArray# :: SmallMutableArray# d a
                   -> State# d -> (# State# d, SmallArray# a #)
unsafeFreezeArray# = unsafeFreezeSmallArray#

readArray# :: SmallMutableArray# d a
           -> Int# -> State# d -> (# State# d, a #)
readArray# = readSmallArray#

writeArray# :: SmallMutableArray# d a
            -> Int# -> a -> State# d -> State# d
writeArray# = writeSmallArray#

indexArray# :: SmallArray# a -> Int# -> (# a #)
indexArray# = indexSmallArray#

unsafeThawArray# :: SmallArray# a
                 -> State# d -> (# State# d, SmallMutableArray# d a #)
unsafeThawArray# = unsafeThawSmallArray#

sizeofArray# :: SmallArray# a -> Int#
sizeofArray# = sizeofSmallArray#

copyArray# :: SmallArray# a
           -> Int#
           -> SmallMutableArray# d a
           -> Int#
           -> Int#
           -> State# d
           -> State# d
copyArray# = copySmallArray#

cloneMutableArray# :: SmallMutableArray# s a
                   -> Int#
                   -> Int#
                   -> State# s
                   -> (# State# s, SmallMutableArray# s a #)
cloneMutableArray# = cloneSmallMutableArray#

thawArray# :: SmallArray# a
           -> Int#
           -> Int#
           -> State# d
           -> (# State# d, SmallMutableArray# d a #)
thawArray# = thawSmallArray#

sizeofMutableArray# :: SmallMutableArray# s a -> Int#
sizeofMutableArray# = sizeofSmallMutableArray#

copyMutableArray# :: SmallMutableArray# d a
                  -> Int#
                  -> SmallMutableArray# d a
                  -> Int#
                  -> Int#
                  -> State# d
                  -> State# d
copyMutableArray# = copySmallMutableArray#



data Array a = Array {
      unArray :: !(Array# a)
    }

instance Show a => Show (Array a) where
    show = show . toList

toList :: Array a -> [a]
toList = foldr (:) []

fromList :: Int -> [a] -> Array a
fromList n xs0 =
    CHECK_EQ("fromList", n, Prelude.length xs0)
        run $ do
            mary <- new_ n
            go xs0 mary 0
  where
    go [] !mary !_   = return mary
    go (x:xs) mary i = do write mary i x
                          go xs mary (i+1)

length :: Array a -> Int
length ary = I# (sizeofArray# (unArray ary))
{-# INLINE length #-}

data MArray s a = MArray {
      unMArray :: !(MutableArray# s a)
    }

instance NFData a => NFData (Array a) where
    rnf = rnfArray

rnfArray :: NFData a => Array a -> ()
rnfArray ary0 = go ary0 n0 0
  where
    n0 = length ary0
    go !ary !n !i
        | i >= n = ()
        | (# x #) <- index# ary i
        = rnf x `seq` go ary n (i+1)
-- We use index# just in case GHC can't see that the
-- relevant rnf is strict, or in case it actually isn't.
{-# INLINE rnfArray #-}

-- | Create a new mutable array of specified size, in the specified
-- state thread, with each element containing the specified initial
-- value.
new :: Int -> a -> ST s (MArray s a)
new (I# n#) b =
    CHECK_GT("new",n,(0 :: Int))
    ST $ \s ->
        case newArray# n# b s of
            (# s', ary #) -> (# s', MArray ary #)
{-# INLINE new #-}

new_ :: Int -> ST s (MArray s a)
new_ n = new n undefinedElem

singleton :: a -> Array a
singleton x = runST (singletonM x)
{-# INLINE singleton #-}

singletonM :: a -> ST s (Array a)
singletonM x = new 1 x >>= unsafeFreeze
{-# INLINE singletonM #-}

pair :: a -> a -> Array a
pair x y = run $ do
    ary <- new 2 x
    write ary 1 y
    return ary
{-# INLINE pair #-}

read :: MArray s a -> Int -> ST s a
read ary _i@(I# i#) = ST $ \ s ->
    CHECK_BOUNDS("read", lengthM ary, _i)
        readArray# (unMArray ary) i# s
{-# INLINE read #-}

write :: MArray s a -> Int -> a -> ST s ()
write ary _i@(I# i#) b = ST $ \ s ->
    CHECK_BOUNDS("write", lengthM ary, _i)
        case writeArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE write #-}

index :: Array a -> Int -> a
index ary _i@(I# i#) =
    CHECK_BOUNDS("index", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE index #-}

index# :: Array a -> Int -> (# a #)
index# ary _i@(I# i#) =
    CHECK_BOUNDS("index#", length ary, _i)
        indexArray# (unArray ary) i#
{-# INLINE index# #-}

indexM :: Array a -> Int -> ST s a
indexM ary _i@(I# i#) =
    CHECK_BOUNDS("indexM", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE indexM #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary
    = ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', Array ary #)
{-# INLINE unsafeFreeze #-}

unsafeThaw :: Array a -> ST s (MArray s a)
unsafeThaw ary
    = ST $ \s -> case unsafeThawArray# (unArray ary) s of
                   (# s', mary #) -> (# s', MArray mary #)
{-# INLINE unsafeThaw #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copy :: Array e -> Int -> MArray s e -> Int -> Int -> ST s ()
copy !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    CHECK_LE("copy", _sidx + _n, length src)
    CHECK_LE("copy", _didx + _n, lengthM dst)
        ST $ \ s# ->
        case copyArray# (unArray src) sidx# (unMArray dst) didx# n# s# of
            s2 -> (# s2, () #)

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copyM :: MArray s e -> Int -> MArray s e -> Int -> Int -> ST s ()
copyM !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    CHECK_BOUNDS("copyM: src", lengthM src, _sidx + _n - 1)
    CHECK_BOUNDS("copyM: dst", lengthM dst, _didx + _n - 1)
    ST $ \ s# ->
    case copyMutableArray# (unMArray src) sidx# (unMArray dst) didx# n# s# of
        s2 -> (# s2, () #)

cloneM :: MArray s a -> Int -> Int -> ST s (MArray s a)
cloneM _mary@(MArray mary#) _off@(I# off#) _len@(I# len#) =
    CHECK_BOUNDS("cloneM_off", lengthM _mary, _off - 1)
    CHECK_BOUNDS("cloneM_end", lengthM _mary, _off + _len - 1)
    ST $ \ s ->
    case cloneMutableArray# mary# off# len# s of
      (# s', mary'# #) -> (# s', MArray mary'# #)

-- | Create a new array of the @n@ first elements of @mary@.
trim :: MArray s a -> Int -> ST s (Array a)
trim mary n = cloneM mary 0 n >>= unsafeFreeze
{-# INLINE trim #-}

insert_new :: Array e -> Int -> e -> Array e
insert_new (Array a#) (I# i#) b = Array (insertSmallArray# a# i# b)
{-# INLINE insert_new #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insert :: Array e -> Int -> e -> Array e
insert ary idx b = runST (insertM ary idx b)
{-# INLINE insert #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: Array e -> Int -> e -> ST s (Array e)
insertM ary idx b =
    CHECK_BOUNDS("insertM", count + 1, idx)
        do mary <- new_ (count+1)
           copy ary 0 mary 0 idx
           write mary idx b
           copy ary idx mary (idx+1) (count-idx)
           unsafeFreeze mary
  where !count = length ary
{-# INLINE insertM #-}

update_new :: Array e -> Int -> e -> Array e
update_new (Array a#) (I# i#) b = Array (updateSmallArray# a# i# b)
{-# INLINE update_new #-}

-- | /O(n)/ Update the element at the given position in this array.
update :: Array e -> Int -> e -> Array e
update ary idx b = runST (updateM ary idx b)
{-# INLINE update #-}

-- | /O(n)/ Update the element at the given position in this array.
updateM :: Array e -> Int -> e -> ST s (Array e)
updateM ary idx b =
    CHECK_BOUNDS("updateM", count, idx)
        do mary <- thaw ary 0 count
           write mary idx b
           unsafeFreeze mary
  where !count = length ary
{-# INLINE updateM #-}

-- | /O(n)/ Update the element at the given positio in this array, by
-- applying a function to it.  Evaluates the element to WHNF before
-- inserting it into the array.
updateWith' :: Array e -> Int -> (e -> e) -> Array e
updateWith' ary idx f
  | (# x #) <- index# ary idx
  = update ary idx $! f x
{-# INLINE updateWith' #-}

-- | /O(1)/ Update the element at the given position in this array,
-- without copying.
unsafeUpdateM :: Array e -> Int -> e -> ST s ()
unsafeUpdateM ary idx b =
    CHECK_BOUNDS("unsafeUpdateM", length ary, idx)
        do mary <- unsafeThaw ary
           write mary idx b
           _ <- unsafeFreeze mary
           return ()
{-# INLINE unsafeUpdateM #-}

foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i !z
        | i >= n = z
        | otherwise
        = case index# ary i of
            (# x #) -> go ary n (i+1) (f z x)
{-# INLINE foldl' #-}

foldr' :: (a -> b -> b) -> b -> Array a -> b
foldr' f = \ z0 ary0 -> go ary0 (length ary0 - 1) z0
  where
    go !_ary (-1) z = z
    go !ary i !z
      | (# x #) <- index# ary i
      = go ary (i - 1) (f x z)
{-# INLINE foldr' #-}

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i z
        | i >= n = z
        | otherwise
        = case index# ary i of
            (# x #) -> f x (go ary n (i+1) z)
{-# INLINE foldr #-}

foldl :: (b -> a -> b) -> b -> Array a -> b
foldl f = \ z0 ary0 -> go ary0 (length ary0 - 1) z0
  where
    go _ary (-1) z = z
    go ary i z
      | (# x #) <- index# ary i
      = f (go ary (i - 1) z) x
{-# INLINE foldl #-}

-- We go to a bit of trouble here to avoid appending an extra mempty.
-- The below implementation is by Mateusz Kowalczyk, who indicates that
-- benchmarks show it to be faster than one that avoids lifting out
-- lst.
foldMap :: Monoid m => (a -> m) -> Array a -> m
foldMap f = \ary0 -> case length ary0 of
  0 -> mempty
  len ->
    let !lst = len - 1
        go i | (# x #) <- index# ary0 i, let fx = f x =
          if i == lst then fx else fx `mappend` go (i + 1)
    in go 0
{-# INLINE foldMap #-}

-- | Verifies that a predicate holds for all elements of an array.
all :: (a -> Bool) -> Array a -> Bool
all p = foldr (\a acc -> p a && acc) True
{-# INLINE all #-}

undefinedElem :: a
undefinedElem = error "Data.HashMap.Internal.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

thaw :: Array e -> Int -> Int -> ST s (MArray s e)
thaw !ary !_o@(I# o#) (I# n#) =
    CHECK_LE("thaw", _o + n, length ary)
        ST $ \ s -> case thawArray# (unArray ary) o# n# s of
            (# s2, mary# #) -> (# s2, MArray mary# #)
{-# INLINE thaw #-}

delete_new :: Array e -> Int -> Array e
delete_new (Array a#) (I# i#) = Array (deleteSmallArray# a# i#)
{-# INLINE delete_new #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
delete :: Array e -> Int -> Array e
delete ary idx = runST (deleteM ary idx)
{-# INLINE delete #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
deleteM :: Array e -> Int -> ST s (Array e)
deleteM ary idx = do
    CHECK_BOUNDS("deleteM", count, idx)
        do mary <- new_ (count-1)
           copy ary 0 mary 0 idx
           copy ary (idx+1) mary idx (count-(idx+1))
           unsafeFreeze mary
  where !count = length ary
{-# INLINE deleteM #-}
