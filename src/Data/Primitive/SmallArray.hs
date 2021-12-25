{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , copySmallArray
  , copySmallMutableArray
  , indexSmallArray
  , indexSmallArrayM
  , indexSmallArray##
  , cloneSmallArray
  , cloneSmallMutableArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , thawSmallArray
  , A.runSmallArray
  , A.unsafeThawSmallArray
  , A.sizeofSmallArray
  , A.sizeofSmallMutableArray
#if MIN_VERSION_base(4,14,0)
  , shrinkSmallMutableArray
#endif
  , A.smallArrayFromList
  , A.smallArrayFromListN
  , A.mapSmallArray'
  , A.traverseSmallArrayP
  ) where

import "primitive" Data.Primitive (sizeOf)
import "primitive" Data.Primitive.SmallArray (SmallArray, SmallMutableArray)

import Control.Exception (throw, ArrayException(..), Exception, toException)
import Control.Monad.Primitive (PrimMonad, PrimState)
import GHC.Exts (raise#)
import GHC.Stack

import qualified "primitive" Data.Primitive.SmallArray as A
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.SmallArray." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

checkUnary :: HasCallStack => String -> Bool -> (# a #) -> (# a #)
checkUnary _      True  x = x
checkUnary errMsg False _ = throwUnary (IndexOutOfBounds $ "Data.Primitive.SmallArray." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

throwUnary :: Exception e => e -> (# a #)
throwUnary e = raise# (toException e)

newSmallArray :: (HasCallStack, PrimMonad m) => Int -> a -> m (SmallMutableArray (PrimState m) a)
newSmallArray n x =
    check "newSmallArray: negative size" (n >= 0)
  $ check ("newSmallArray: requested " ++ show n ++ " elements") (n * ptrSz < 1024*1024*1024)
  $ A.newSmallArray n x
  where
  ptrSz = sizeOf (undefined :: Int)

readSmallArray :: (HasCallStack, PrimMonad m) => SmallMutableArray (PrimState m) a -> Int -> m a
readSmallArray marr i = do
  let siz = A.sizeofSmallMutableArray marr
      explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("readSmallArray: index out of bounds " ++ explain) (i >= 0 && i < siz) (A.readSmallArray marr i)

writeSmallArray :: (HasCallStack, PrimMonad m) => SmallMutableArray (PrimState m) a -> Int -> a -> m ()
writeSmallArray marr i x = do
  let siz = A.sizeofSmallMutableArray marr
      explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("writeSmallArray: index out of bounds " ++ explain) (i >= 0 && i < siz) (A.writeSmallArray marr i x)

indexSmallArray :: HasCallStack => SmallArray a -> Int -> a
indexSmallArray arr i = check ("indexSmallArray: index out of bounds " ++ explain)
  (i >= 0 && i < A.sizeofSmallArray arr)
  (A.indexSmallArray arr i)
  where
  explain = L.concat
    [ "[size: "
    , show (A.sizeofSmallArray arr)
    , ", index: "
    , show i
    , "]"
    ]

indexSmallArray## :: HasCallStack => SmallArray a -> Int -> (# a #)
indexSmallArray## arr i = checkUnary "indexSmallArray##: index out of bounds"
  (i >= 0 && i < A.sizeofSmallArray arr)
  (A.indexSmallArray## arr i)

indexSmallArrayM :: (HasCallStack, Monad m) => SmallArray a -> Int -> m a
indexSmallArrayM arr i = check "indexSmallArrayM: index out of bounds"
  (i >= 0 && i < A.sizeofSmallArray arr)
  (A.indexSmallArrayM arr i)

{-# NOINLINE errorUnsafeFreeze #-}
errorUnsafeFreeze :: a
errorUnsafeFreeze =
  error "Data.Primitive.Array.unsafeFreeze:\nAttempted to read from an array after unsafely freezing it."

-- | This installs error thunks in the argument array so that
-- any attempt to use it after an unsafeFreeze will fail.
unsafeFreezeSmallArray :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a
  -> m (SmallArray a)
unsafeFreezeSmallArray marr = do
  let sz = A.sizeofSmallMutableArray marr
  arr <- A.freezeSmallArray marr 0 sz
  let go !ix = if ix < sz
        then A.writeSmallArray marr ix errorUnsafeFreeze >> go (ix + 1)
        else return ()
  go 0
  return arr

freezeSmallArray
  :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallArray a)
freezeSmallArray marr s l = check "freezeSmallArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofSmallMutableArray marr)
  (A.freezeSmallArray marr s l)

thawSmallArray
  :: (HasCallStack, PrimMonad m)
  => SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
thawSmallArray arr s l = check "thawSmallArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofSmallArray arr)
  (A.thawSmallArray arr s l)

copySmallArray :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a -- ^ destination array
  -> Int                               -- ^ offset into destination array
  -> SmallArray a                      -- ^ source array
  -> Int                               -- ^ offset into source array
  -> Int                               -- ^ number of elements to copy
  -> m ()
copySmallArray marr s1 arr s2 l = do
  let siz = A.sizeofSmallMutableArray marr
  check "copySmallArray: index range of out bounds"
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz && s2 + l <= A.sizeofSmallArray arr)
    (A.copySmallArray marr s1 arr s2 l)

copySmallMutableArray :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a -- ^ destination array
  -> Int                               -- ^ offset into destination array
  -> SmallMutableArray (PrimState m) a -- ^ source array
  -> Int                               -- ^ offset into source array
  -> Int                               -- ^ number of elements to copy
  -> m ()
copySmallMutableArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofSmallMutableArray marr1
  let siz2 = A.sizeofSmallMutableArray marr2
  check "copySmallMutableArray: index range of out bounds"
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz1 && s2 + l <= siz2)
    (A.copySmallMutableArray marr1 s1 marr2 s2 l)

cloneSmallArray :: HasCallStack
  => SmallArray a -- ^ source array
  -> Int          -- ^ offset into source array
  -> Int          -- ^ number of elements to copy
  -> SmallArray a
cloneSmallArray arr s l = check "cloneSmallArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofSmallArray arr)
  (A.cloneSmallArray arr s l)

cloneSmallMutableArray :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a -- ^ source array
  -> Int                               -- ^ offset into source array
  -> Int                               -- ^ number of elements to copy
  -> m (SmallMutableArray (PrimState m) a)
cloneSmallMutableArray marr s l = check "cloneSmallMutableArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofSmallMutableArray marr)
  (A.cloneSmallMutableArray marr s l)

#if MIN_VERSION_base(4,14,0)
shrinkSmallMutableArray :: (HasCallStack, PrimMonad m) => SmallMutableArray (PrimState m) a -> Int -> m ()
shrinkSmallMutableArray marr n = do
    let old = A.sizeofSmallMutableArray marr
    check "shrinkSmallMutableArray: illegal new size" (n >= 0 && n <= old) (A.shrinkSmallMutableArray marr n)
#endif
