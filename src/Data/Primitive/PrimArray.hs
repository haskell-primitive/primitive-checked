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
  , A.unsafeFreezePrimArray
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
import Data.Primitive.Types (Prim)
import "primitive" Data.Primitive.PrimArray (PrimArray,MutablePrimArray)
import qualified "primitive" Data.Primitive.PrimArray as A

check :: String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.PrimArray." ++ errMsg)

newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray n = check "newPrimArray: negative size" (n>=0) (A.newPrimArray n)

resizeMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m (MutablePrimArray (PrimState m) a)
resizeMutablePrimArray marr n = check "resizeMutablePrimArray: negative size" (n>=0) (A.resizeMutablePrimArray marr n)

#if __GLASGOW_HASKELL__ >= 710
shrinkMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m ()
shrinkMutablePrimArray marr n = do
  old <- A.getSizeofMutablePrimArray marr
  check "shrinkMutablePrimArray: illegal new size" (n>=0 && n <= old) (A.shrinkMutablePrimArray marr n)
#endif

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray marr i = do
  siz <- A.getSizeofMutablePrimArray marr
  check "readPrimArray: index of out bounds" (i>=0 && i<siz) (A.readPrimArray marr i)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
writePrimArray marr i x = do
  siz <- A.getSizeofMutablePrimArray marr
  check "writePrimArray: index of out bounds" (i>=0 && i<siz) (A.writePrimArray marr i x)

indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
indexPrimArray arr i = check "indexPrimArray: index of out bounds"
  (i>=0 && i< A.sizeofPrimArray arr)
  (A.indexPrimArray arr i)

setPrimArray :: forall m a. (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
setPrimArray dst doff sz x = do
  arrSz <- A.getSizeofMutablePrimArray dst
  check "copyMutablePrimArray: index range of out bounds"
    (doff>=0 && (doff+sz)<=arrSz)
    (A.setPrimArray dst doff sz x)

copyMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
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
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyPrimArray marr s1 arr s2 l = do
  siz <- A.getSizeofMutablePrimArray marr
  check "copyPrimArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<= A.sizeofPrimArray arr && (s1+l)<=siz)
    (A.copyPrimArray marr s1 arr s2 l)

