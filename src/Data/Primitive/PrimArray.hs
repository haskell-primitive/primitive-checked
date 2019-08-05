{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.PrimArray
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
  , resizeMutablePrimArray
#if __GLASGOW_HASKELL__ >= 710
  , shrinkMutablePrimArray
#endif
    -- * Element Access
  , readPrimArray
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , unsafeFreezePrimArray
  , A.unsafeThawPrimArray
    -- * Block Operations
  , copyPrimArray
  , copyMutablePrimArray
#if __GLASGOW_HASKELL__ >= 708
  , A.copyPrimArrayToPtr -- this is wrong. fix this
  , A.copyMutablePrimArrayToPtr -- this is wrong. fix this
#endif
  , setPrimArray
    -- * Information
  , A.sameMutablePrimArray
  , A.getSizeofMutablePrimArray
  , A.sizeofMutablePrimArray
  , A.sizeofPrimArray
    -- * Folding
  , A.foldrPrimArray
  , A.foldrPrimArray'
  , A.foldlPrimArray
  , A.foldlPrimArray'
  , A.foldlPrimArrayM'
    -- * Effectful Folding
  , A.traversePrimArray_
  , A.itraversePrimArray_
    -- * Map/Create
  , A.mapPrimArray
  , A.imapPrimArray
  , A.generatePrimArray
  , A.replicatePrimArray
  , A.filterPrimArray
  , A.mapMaybePrimArray
    -- * Effectful Map/Create
    -- $effectfulMapCreate
    -- ** Lazy Applicative
  , A.traversePrimArray
  , A.itraversePrimArray
  , A.generatePrimArrayA
  , A.replicatePrimArrayA
  , A.filterPrimArrayA
  , A.mapMaybePrimArrayA
    -- ** Strict Primitive Monadic
  , A.traversePrimArrayP
  , A.itraversePrimArrayP
  , A.generatePrimArrayP
  , A.replicatePrimArrayP
  , A.filterPrimArrayP
  , A.mapMaybePrimArrayP
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Exception (throw, ArrayException(..))
import Data.Primitive.Types (Prim,sizeOf)
import Data.Word (Word8)
import "primitive" Data.Primitive.PrimArray (PrimArray,MutablePrimArray)
import qualified "primitive" Data.Primitive.PrimArray as A
import GHC.Stack
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.PrimArray." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

newPrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray n =
    check "newPrimArray: negative size" (n>=0)
  $ check ("newPrimArray: requested " ++ show n ++ " elements of size " ++ show elemSz) (n * elemSz < 1024*1024*1024)
  $ A.newPrimArray n
  where
  elemSz = sizeOf (undefined :: a)

-- | After a call to resizeMutablePrimArray, the original reference to
-- the mutable array should not be used again. This cannot truly be enforced
-- except by linear types. To attempt to enforce this, we always make a
-- copy of the mutable byte array and intentionally corrupt the original
-- of the original one. The strategy used here to corrupt the array is
-- simply to write 1 to every bit.
resizeMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m (MutablePrimArray (PrimState m) a)
resizeMutablePrimArray marr@(A.MutablePrimArray x) n = check "resizeMutablePrimArray: negative size" (n>=0) $ do
  sz <- A.getSizeofMutablePrimArray marr
  marr' <- A.newPrimArray n
  A.copyMutablePrimArray marr' 0 marr 0 (min sz n)
  A.setPrimArray (A.MutablePrimArray x) 0 (sz * sizeOf (undefined :: a)) (0xFF :: Word8)
  return marr'

-- | This corrupts the contents of the argument array.
unsafeFreezePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> m (PrimArray a)
unsafeFreezePrimArray marr@(A.MutablePrimArray x) = do
  sz <- A.getSizeofMutablePrimArray marr
  marr' <- A.newPrimArray sz
  A.copyMutablePrimArray marr' 0 marr 0 sz
  A.setPrimArray (A.MutablePrimArray x) 0 (sz * sizeOf (undefined :: a)) (0xFF :: Word8)
  A.unsafeFreezePrimArray marr'

#if __GLASGOW_HASKELL__ >= 710
shrinkMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m ()
shrinkMutablePrimArray marr n = do
  old <- A.getSizeofMutablePrimArray marr
  check "shrinkMutablePrimArray: illegal new size" (n>=0 && n <= old) (A.shrinkMutablePrimArray marr n)
#endif

readPrimArray :: (HasCallStack, Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray marr i = do
  siz <- A.getSizeofMutablePrimArray marr
  let explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("readPrimArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.readPrimArray marr i)

writePrimArray ::
     (HasCallStack, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
writePrimArray marr i x = do
  siz <- A.getSizeofMutablePrimArray marr
  let explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("writePrimArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.writePrimArray marr i x)

indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
indexPrimArray arr i = 
  let sz = A.sizeofPrimArray arr
      explain = L.concat
        [ "[size: "
        , show sz
        , ", index: "
        , show i
        , "]"
        ]
   in check ("indexPrimArray: index of out bounds " ++ explain)
        (i>=0 && i< sz)
        (A.indexPrimArray arr i)

setPrimArray :: forall m a. (HasCallStack, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
setPrimArray dst doff sz x = do
  arrSz <- A.getSizeofMutablePrimArray dst
  let explain = L.concat
        [ "[size: "
        , show arrSz
        , ", offset: "
        , show doff
        , ", length: "
        , show sz
        , "]"
        ]
  check ("setPrimArray: index range of out bounds " ++ explain)
    (doff>=0 && (doff+sz)<=arrSz)
    (A.setPrimArray dst doff sz x)

copyMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutablePrimArray marr1 s1 marr2 s2 l = do
  siz1 <- A.getSizeofMutablePrimArray marr1
  siz2 <- A.getSizeofMutablePrimArray marr2
  check "copyMutablePrimArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (A.copyMutablePrimArray marr1 s1 marr2 s2 l)

copyPrimArray :: forall m a.
     (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyPrimArray marr s1 arr s2 l = do
  dstSz <- A.getSizeofMutablePrimArray marr
  let srcSz = A.sizeofPrimArray arr
  let explain = L.concat
        [ "[dst_sz: "
        , show dstSz
        , ", dst_off: "
        , show s1
        , ", src_sz: "
        , show srcSz
        , ", src_off: "
        , show s2
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyPrimArray: index range of out bounds " ++ explain)
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<= srcSz && (s1+l)<=dstSz)
    (A.copyPrimArray marr s1 arr s2 l)

