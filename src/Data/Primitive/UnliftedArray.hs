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
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Exception (throw, ArrayException(..))
import "primitive" Data.Primitive.UnliftedArray (UnliftedArray,MutableUnliftedArray,PrimUnlifted)
import qualified "primitive" Data.Primitive.UnliftedArray as A

check :: String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.UnliftedArray.Checked." ++ errMsg)

newUnliftedArray :: (PrimMonad m, PrimUnlifted a) => Int -> a -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray n x = check "newUnliftedArray: negative size" (n>=0) (A.newUnliftedArray n x)

unsafeNewUnliftedArray :: PrimMonad m => Int -> m (MutableUnliftedArray (PrimState m) a)
unsafeNewUnliftedArray n = check "unsafeNewUnliftedArray: negative size" (n>=0) (A.unsafeNewUnliftedArray n)

readUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> m a
readUnliftedArray marr i = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "readUnliftedArray: index of out bounds" (i>=0 && i<siz) (A.readUnliftedArray marr i)

writeUnliftedArray :: (PrimMonad m, PrimUnlifted a) => MutableUnliftedArray (PrimState m) a -> Int -> a -> m ()
writeUnliftedArray marr i x = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "writeUnliftedArray: index of out bounds" (i>=0 && i<siz) (A.writeUnliftedArray marr i x)

indexUnliftedArray :: PrimUnlifted a => UnliftedArray a -> Int -> a
indexUnliftedArray arr i = check "indexUnliftedArray: index of out bounds"
  (i>=0 && i<A.sizeofUnliftedArray arr)
  (A.indexUnliftedArray arr i)

indexUnliftedArrayM :: (Monad m, PrimUnlifted a) => UnliftedArray a -> Int -> m a
indexUnliftedArrayM arr i = check "indexUnliftedArrayM: index of out bounds"
    (i>=0 && i<A.sizeofUnliftedArray arr)
    (A.indexUnliftedArrayM arr i)

freezeUnliftedArray
  :: PrimMonad m
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
  :: PrimMonad m
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
thawUnliftedArray arr s l = check "thawArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofUnliftedArray arr)
    (A.thawUnliftedArray arr s l)

copyUnliftedArray :: PrimMonad m
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


copyMutableUnliftedArray :: PrimMonad m
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


cloneUnliftedArray ::
     UnliftedArray a -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of elements to copy
  -> UnliftedArray a
cloneUnliftedArray arr s l = check "cloneUnliftedArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofUnliftedArray arr)
    (A.cloneUnliftedArray arr s l)

cloneMutableUnliftedArray :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into destination array
  -> Int -- ^ number of elements to copy
  -> m (MutableUnliftedArray (PrimState m) a)
cloneMutableUnliftedArray marr s l = do
  let siz = A.sizeofMutableUnliftedArray marr
  check "cloneMutableUnliftedArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (A.cloneMutableUnliftedArray marr s l)

