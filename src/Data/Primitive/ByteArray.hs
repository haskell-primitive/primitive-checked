{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.ByteArray
  ( -- * Types
    A.ByteArray(..)
  , A.MutableByteArray(..)
  , A.ByteArray#
  , A.MutableByteArray#
    -- * Allocation
  , newByteArray
  , newPinnedByteArray
  , newAlignedPinnedByteArray
    -- * Element access
  , readByteArray
  , writeByteArray
  , indexByteArray
    -- * Folding
  , A.foldrByteArray
    -- * Freezing and thawing
  , A.unsafeFreezeByteArray
  , A.unsafeThawByteArray
    -- * Block operations
  , copyByteArray
  , copyMutableByteArray
  , moveByteArray
  , setByteArray
  , fillByteArray
  -- * Information
  , A.sizeofByteArray
  , A.sizeofMutableByteArray
  , A.sameMutableByteArray
  , A.byteArrayContents
  , A.mutableByteArrayContents
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Exception (throw, ArrayException(..))
import Data.Primitive.Types (Prim,sizeOf)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import "primitive" Data.Primitive.ByteArray (ByteArray,MutableByteArray)
import qualified "primitive" Data.Primitive.ByteArray as A

check :: String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.SmallArray.Checked." ++ errMsg)

elementSizeofByteArray :: forall a. Prim a => Proxy a -> ByteArray -> Int
elementSizeofByteArray _ arr = div (A.sizeofByteArray arr) (sizeOf (undefined :: a))

elementSizeofMutableByteArray :: forall s a. Prim a => Proxy a -> MutableByteArray s -> Int
elementSizeofMutableByteArray _ arr = div (A.sizeofMutableByteArray arr) (sizeOf (undefined :: a))

newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newByteArray n = check "newByteArray: negative size" (n>=0) (A.newByteArray n)

newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newPinnedByteArray n = check "newPinnedByteArray: negative size" (n>=0) (A.newPinnedByteArray n)

newAlignedPinnedByteArray :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
newAlignedPinnedByteArray n k = check "newAlignedPinnedByteArray: negative size" (n>=0) (A.newAlignedPinnedByteArray n k)

readByteArray :: forall m a. (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
readByteArray marr i = do
  let siz = elementSizeofMutableByteArray (Proxy :: Proxy a) marr
  check "readByteArray: index of out bounds" (i>=0 && i<siz) (A.readByteArray marr i)

writeByteArray :: forall m a. (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeByteArray marr i x = do
  let siz = elementSizeofMutableByteArray (Proxy :: Proxy a) marr
  check "writeByteArray: index of out bounds" (i>=0 && i<siz) (A.writeByteArray marr i x)

indexByteArray :: forall a. Prim a => ByteArray -> Int -> a
indexByteArray arr i = check "indexSmallArray: index of out bounds"
  (i>=0 && i< elementSizeofByteArray (Proxy :: Proxy a) arr)
  (A.indexByteArray arr i)

copyByteArray :: forall m. PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyByteArray marr s1 arr s2 l = do
  let siz = A.sizeofMutableByteArray marr
  check "copyByteArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<= A.sizeofByteArray arr && (s1+l)<=siz)
    (A.copyByteArray marr s1 arr s2 l)


copyMutableByteArray :: forall m. PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutableByteArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofMutableByteArray marr1
  let siz2 = A.sizeofMutableByteArray marr2
  check "copyMutableByteArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (A.copyMutableByteArray marr1 s1 marr2 s2 l)

moveByteArray :: forall m. PrimMonad m
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
moveByteArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofMutableByteArray marr1
  let siz2 = A.sizeofMutableByteArray marr2
  check "moveByteArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (A.moveByteArray marr1 s1 marr2 s2 l)

fillByteArray :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of bytes to fill
  -> Word8 -- ^ byte to fill with
  -> m ()
fillByteArray = setByteArray

setByteArray :: forall m a. (Prim a, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
{-# INLINE setByteArray #-}
setByteArray dst doff sz x  = 
  check "copyMutableByteArray: index range of out bounds"
    (doff>=0 && (doff+sz)<=elementSizeofMutableByteArray (Proxy :: Proxy a) dst)
    (A.setByteArray dst doff sz x)

