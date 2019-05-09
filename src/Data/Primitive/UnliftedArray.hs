{-# LANGUAGE PackageImports #-}

module Data.Primitive.UnliftedArray
  ( UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted(..)
  , unsafeNewUnliftedArray
  , newUnliftedArray
  , A.setUnliftedArray
  , A.sizeofUnliftedArray
  , A.sizeofMutableUnliftedArray
  , readUnliftedArray
  , writeUnliftedArray
  , indexUnliftedArray
  , indexUnliftedArrayM
  , A.unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , A.sameMutableUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , cloneUnliftedArray
  , cloneMutableUnliftedArray
  , A.mapUnliftedArray
  , A.foldrUnliftedArray
  , A.foldlUnliftedArray
  , A.foldrUnliftedArray'
  , A.foldlUnliftedArray'
  , A.unliftedArrayFromList
  ) where

import "primitive" Data.Primitive (sizeOf)
import "primitive" Data.Primitive.UnliftedArray (UnliftedArray,MutableUnliftedArray,PrimUnlifted)

import Control.Exception (throw, ArrayException(..))
import Control.Monad.Primitive (PrimMonad,PrimState)
import GHC.Stack

import qualified "primitive" Data.Primitive.UnliftedArray as A
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.UnliftedArray.Checked." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

newUnliftedArray :: (HasCallStack, PrimMonad m, PrimUnlifted a) => Int -> a -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray n x =
    check "newUnliftedArray: negative size" (n>=0)
  $ check ("newUnliftedArray: requested " ++ show n ++ " elements") (n * ptrSz < 1024*1024*1024)
  $ A.newUnliftedArray n x
  where
  ptrSz = sizeOf (undefined :: Int)

unsafeNewUnliftedArray :: (HasCallStack, PrimMonad m) => Int -> m (MutableUnliftedArray (PrimState m) a)
unsafeNewUnliftedArray n =
    check "unsafeNewUnliftedArray: negative size" (n>=0)
  $ check ("unsafeNewUnliftedArray: requested " ++ show n ++ " elements") (n * ptrSz < 1024*1024*1024)
  $ A.unsafeNewUnliftedArray n
  where
  ptrSz = sizeOf (undefined :: Int)

readUnliftedArray :: (HasCallStack, PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m a
readUnliftedArray marr i = do
  let siz = A.sizeofMutableUnliftedArray marr
      explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("readUnliftedArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.readUnliftedArray marr i)

writeUnliftedArray :: (HasCallStack, PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> a -> m ()
writeUnliftedArray marr i x = do
  let siz = A.sizeofMutableUnliftedArray marr
      explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , "]"
        ]
  check ("writeUnliftedArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.writeUnliftedArray marr i x)

indexUnliftedArray :: (HasCallStack, PrimUnlifted a) => UnliftedArray a -> Int -> a
indexUnliftedArray arr i =
  let sz = A.sizeofUnliftedArray arr
      explain = L.concat
        [ "[size: "
        , show sz
        , ", index: "
        , show i
        , "]"
        ]
   in check ("indexUnliftedArray: index of out bounds " ++ explain)
        (i>=0 && i<sz)
        (A.indexUnliftedArray arr i)

indexUnliftedArrayM :: (HasCallStack, Monad m, PrimUnlifted a) => UnliftedArray a -> Int -> m a
indexUnliftedArrayM arr i = check "indexUnliftedArrayM: index of out bounds"
    (i>=0 && i<A.sizeofUnliftedArray arr)
    (A.indexUnliftedArrayM arr i)

freezeUnliftedArray
  :: (HasCallStack, PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (UnliftedArray a)
freezeUnliftedArray marr s l = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "freezeUnliftedArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (A.freezeUnliftedArray marr s l)

thawUnliftedArray
  :: (HasCallStack, PrimMonad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
thawUnliftedArray arr s l = check "thawArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofUnliftedArray arr)
    (A.thawUnliftedArray arr s l)

copyUnliftedArray :: (HasCallStack, PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> UnliftedArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyUnliftedArray marr s1 arr s2 l = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "copyUnliftedArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofUnliftedArray arr && (s1+l)<=siz)
    (A.copyUnliftedArray marr s1 arr s2 l)


copyMutableUnliftedArray :: (HasCallStack, PrimMonad m)
  => MutableUnliftedArray (PrimState m) a    -- ^ destination array
  -> Int                             -- ^ offset into destination array
  -> MutableUnliftedArray (PrimState m) a    -- ^ source array
  -> Int                             -- ^ offset into source array
  -> Int                             -- ^ number of elements to copy
  -> m ()
copyMutableUnliftedArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofMutableUnliftedArray marr1
  let siz2 = A.sizeofMutableUnliftedArray marr2
  check "copyMutableUnliftedArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (A.copyMutableUnliftedArray marr1 s1 marr2 s2 l)


cloneUnliftedArray :: HasCallStack
  => UnliftedArray a -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of elements to copy
  -> UnliftedArray a
cloneUnliftedArray arr s l = check "cloneUnliftedArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofUnliftedArray arr)
    (A.cloneUnliftedArray arr s l)

cloneMutableUnliftedArray :: (HasCallStack, PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of elements to copy
  -> m (MutableUnliftedArray (PrimState m) a)
cloneMutableUnliftedArray marr s l = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "cloneMutableUnliftedArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (A.cloneMutableUnliftedArray marr s l)

