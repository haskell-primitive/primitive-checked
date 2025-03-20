{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Array
  ( Array(..)
  , MutableArray(..)
  , newArray
  , readArray
  , writeArray
  , indexArray
  , indexArrayM
  , indexArray##
  , freezeArray
  , thawArray
  , A.runArray
  , createArray
  , unsafeFreezeArray
  , A.unsafeThawArray
  , A.sameMutableArray
  , copyArray
  , copyMutableArray
  , cloneArray
  , cloneMutableArray
  , A.sizeofArray
  , A.sizeofMutableArray
  , A.emptyArray
  , G.fromListN
  , G.fromList
  , A.arrayFromListN
  , A.arrayFromList
  , A.mapArray'
  , A.traverseArrayP
  ) where

import Control.Exception (throw, ArrayException(..), Exception, toException)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import qualified Data.List as L
import "primitive" Data.Primitive.Array (Array, MutableArray)
import qualified "primitive" Data.Primitive.Array as A
import GHC.Exts (raise#)
import GHC.IsList as G (fromListN, fromList)
import GHC.Stack

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.Array." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

checkUnary :: HasCallStack => String -> Bool -> (# a #) -> (# a #)
checkUnary _      True  x = x
checkUnary errMsg False _ = throwUnary (IndexOutOfBounds $ "Data.Primitive.Array." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

throwUnary :: Exception e => e -> (# a #)
throwUnary e = raise# (toException e)

newArray :: (HasCallStack, PrimMonad m) => Int -> a -> m (MutableArray (PrimState m) a)
newArray n x = check "newArray: negative size" (n >= 0) (A.newArray n x)

readArray :: (HasCallStack, PrimMonad m) => MutableArray (PrimState m) a -> Int -> m a
readArray marr i = do
  let siz = A.sizeofMutableArray marr
  check "readArray: index out of bounds" (i >= 0 && i < siz) (A.readArray marr i)

writeArray :: (HasCallStack, PrimMonad m) => MutableArray (PrimState m) a -> Int -> a -> m ()
writeArray marr i x = do
  let siz = A.sizeofMutableArray marr
  check "writeArray: index out of bounds" (i >= 0 && i < siz) (A.writeArray marr i x)

indexArray :: HasCallStack => Array a -> Int -> a
indexArray arr i = check "indexArray: index out of bounds"
  (i >= 0 && i < A.sizeofArray arr)
  (A.indexArray arr i)

indexArrayM :: (HasCallStack, Monad m) => Array a -> Int -> m a
indexArrayM arr i = check "indexArrayM: index out of bounds"
  (i >= 0 && i < A.sizeofArray arr)
  (A.indexArrayM arr i)

indexArray## :: HasCallStack => Array a -> Int -> (# a #)
indexArray## arr i = checkUnary "indexArray##: index out of bounds"
  (i >= 0 && i < A.sizeofArray arr)
  (A.indexArray## arr i)

{-# NOINLINE errorUnsafeFreeze #-}
errorUnsafeFreeze :: a
errorUnsafeFreeze =
  error "Data.Primitive.Array.unsafeFreeze:\nAttempted to read from an array after unsafely freezing it."

-- | This installs error thunks in the argument array so that
-- any attempt to use it after an unsafeFreeze will fail.
unsafeFreezeArray
  :: (HasCallStack, PrimMonad m)
  => MutableArray (PrimState m) a
  -> m (Array a)
unsafeFreezeArray marr = do
  let sz = A.sizeofMutableArray marr
  arr <- A.freezeArray marr 0 sz
  let go !ix = if ix < sz
        then A.writeArray marr ix errorUnsafeFreeze >> go (ix + 1)
        else return ()
  go 0
  return arr

freezeArray
  :: (HasCallStack, PrimMonad m)
  => MutableArray (PrimState m) a -- ^ source
  -> Int                          -- ^ offset
  -> Int                          -- ^ length
  -> m (Array a)
freezeArray marr s l = check "freezeArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutableArray marr)
  (A.freezeArray marr s l)

thawArray
  :: (HasCallStack, PrimMonad m)
  => Array a -- ^ source
  -> Int     -- ^ offset
  -> Int     -- ^ length
  -> m (MutableArray (PrimState m) a)
thawArray arr s l = check "thawArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofArray arr)
  (A.thawArray arr s l)

createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray n x f = check "createArray: negative size"
  (n >= 0)
  (A.createArray n x f)

copyArray
  :: (HasCallStack, PrimMonad m)
  => MutableArray (PrimState m) a -- ^ destination array
  -> Int                          -- ^ offset into destination array
  -> Array a                      -- ^ source array
  -> Int                          -- ^ offset into source array
  -> Int                          -- ^ number of elements to copy
  -> m ()
copyArray marr s1 arr s2 l = do
  let siz = A.sizeofMutableArray marr
  check "copyArray: index range of out bounds"
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz && s2 + l <= A.sizeofArray arr)
    (A.copyArray marr s1 arr s2 l)

copyMutableArray
  :: (HasCallStack, PrimMonad m)
  => MutableArray (PrimState m) a -- ^ destination array
  -> Int                          -- ^ offset into destination array
  -> MutableArray (PrimState m) a -- ^ source array
  -> Int                          -- ^ offset into source array
  -> Int                          -- ^ number of elements to copy
  -> m ()
copyMutableArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofMutableArray marr1
  let siz2 = A.sizeofMutableArray marr2
  let explain = L.concat
        [ "[dst size: "
        , show siz1
        , ", dst off: "
        , show s1
        , ", src size: "
        , show siz2
        , ", src off: "
        , show s2
        , ", copy size: "
        , show l
        , "]"
        ]
  check ("copyMutableArray: index range of out bounds " ++ explain)
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz1 && s2 + l <= siz2)
    (A.copyMutableArray marr1 s1 marr2 s2 l)

cloneArray
  :: HasCallStack
  => Array a -- ^ source array
  -> Int     -- ^ offset into source array
  -> Int     -- ^ number of elements to copy
  -> Array a
cloneArray arr s l = check "cloneArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofArray arr)
  (A.cloneArray arr s l)

cloneMutableArray
  :: (HasCallStack, PrimMonad m)
  => MutableArray (PrimState m) a -- ^ source array
  -> Int                          -- ^ offset into source array
  -> Int                          -- ^ number of elements to copy
  -> m (MutableArray (PrimState m) a)
cloneMutableArray marr s l = check "cloneMutableArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutableArray marr)
  (A.cloneMutableArray marr s l)
