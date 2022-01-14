{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.ByteArray
  ( -- * Types
    ByteArray(..)
  , MutableByteArray(..)
  , A.ByteArray#
  , A.MutableByteArray#
    -- * Allocation
  , newByteArray
  , newPinnedByteArray
  , newAlignedPinnedByteArray
  , resizeMutableByteArray
  , shrinkMutableByteArray
    -- * Element access
  , readByteArray
  , writeByteArray
  , indexByteArray
    -- * Constructing
  , A.emptyByteArray
  , A.byteArrayFromList
  , A.byteArrayFromListN
    -- * Folding
  , A.foldrByteArray
    -- * Comparing
  , compareByteArrays
    -- * Freezing and thawing
  , freezeByteArray
  , thawByteArray
  , A.runByteArray
  , A.unsafeFreezeByteArray
  , A.unsafeThawByteArray
    -- * Block operations
  , copyByteArray
  , copyMutableByteArray
  , copyByteArrayToPtr
  , copyMutableByteArrayToPtr
  , copyByteArrayToAddr
  , copyMutableByteArrayToAddr
  , moveByteArray
  , setByteArray
  , fillByteArray
  , cloneByteArray
  , cloneMutableByteArray
  -- * Information
  , A.sizeofByteArray
  , A.sizeofMutableByteArray
  , A.getSizeofMutableByteArray
  , A.sameMutableByteArray
  , A.isByteArrayPinned
  , A.isMutableByteArrayPinned
  , A.byteArrayContents
  , A.mutableByteArrayContents
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Exception (throw, ArrayException(..))
import Data.Primitive.Types (Prim, Ptr, sizeOf)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import "primitive" Data.Primitive.ByteArray (ByteArray, MutableByteArray)
import qualified "primitive" Data.Primitive.ByteArray as A
import qualified Data.List as L
import GHC.Stack

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.ByteArray." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

elementSizeofByteArray :: forall a. Prim a => Proxy a -> ByteArray -> Int
elementSizeofByteArray _ arr = A.sizeofByteArray arr `div` sizeOf (undefined :: a)

getElementSizeofMutableByteArray :: forall m a. (PrimMonad m, Prim a)
  => Proxy a -> MutableByteArray (PrimState m) -> m Int
getElementSizeofMutableByteArray _ arr = do
  sz <- A.getSizeofMutableByteArray arr
  return (sz `div` sizeOf (undefined :: a))

newByteArray :: (HasCallStack, PrimMonad m) => Int -> m (MutableByteArray (PrimState m))
newByteArray n =
    check "newByteArray: negative size" (n >= 0)
  $ check ("newByteArray: requested " ++ show n ++ " bytes") (n < 1024*1024*1024)
  $ A.newByteArray n

newPinnedByteArray :: (HasCallStack, PrimMonad m) => Int -> m (MutableByteArray (PrimState m))
newPinnedByteArray n = check "newPinnedByteArray: negative size" (n >= 0) (A.newPinnedByteArray n)

newAlignedPinnedByteArray :: (HasCallStack, PrimMonad m) => Int -> Int -> m (MutableByteArray (PrimState m))
newAlignedPinnedByteArray n k = check "newAlignedPinnedByteArray: negative size" (n >= 0) (A.newAlignedPinnedByteArray n k)

resizeMutableByteArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m (MutableByteArray (PrimState m))
resizeMutableByteArray a n = check "resizeMutableByteArray: negative size" (n >= 0) (A.resizeMutableByteArray a n)

shrinkMutableByteArray :: (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m)
  -> Int -- ^ new size
  -> m ()
shrinkMutableByteArray marr n = do
  old <- A.getSizeofMutableByteArray marr
  check "shrinkMutableByteArray: illegal new size" (n >= 0 && n <= old) (A.shrinkMutableByteArray marr n)

readByteArray :: forall m a. (HasCallStack, Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
readByteArray marr i = do
  siz <- getElementSizeofMutableByteArray (Proxy :: Proxy a) marr
  check "readByteArray: index out of bounds" (i >= 0 && i < siz) (A.readByteArray marr i)

writeByteArray :: forall m a. (HasCallStack, Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeByteArray marr i x = do
  siz <- getElementSizeofMutableByteArray (Proxy :: Proxy a) marr
  let explain = L.concat
        [ "[size: "
        , show siz
        , ", index: "
        , show i
        , ", elem_sz: "
        , show (sizeOf (undefined :: a))
        , "]"
        ]
  check ("writeByteArray: index out of bounds " ++ explain)
    (i >= 0 && i < siz)
    (A.writeByteArray marr i x)

-- This one is a little special. We allow users to index past the
-- end of the byte array as long as the content grabbed is within
-- the last machine word of the byte array.
indexByteArray :: forall a. (HasCallStack, Prim a) => ByteArray -> Int -> a
indexByteArray arr i = check "indexByteArray: index out of bounds"
  (i >= 0 && i < elementSizeofByteArray (Proxy :: Proxy a) arr)
  (A.indexByteArray arr i)

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
compareByteArrays arr1 off1 arr2 off2 len = check "compareByteArrays: index range out of bounds"
  (off1 >= 0 && off2 >= 0 && off1 + len <= A.sizeofByteArray arr1 && off2 + len <= A.sizeofByteArray arr2)
  (A.compareByteArrays arr1 off1 arr2 off2 len)

freezeByteArray
  :: (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ source
  -> Int                            -- ^ offset
  -> Int                            -- ^ length
  -> m ByteArray
freezeByteArray marr s l = check "freezeByteArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutableByteArray marr)
  (A.freezeByteArray marr s l)

thawByteArray
  :: (HasCallStack, PrimMonad m)
  => ByteArray -- ^ source
  -> Int       -- ^ offset
  -> Int       -- ^ length
  -> m (MutableByteArray (PrimState m))
thawByteArray arr s l = check "thawByteArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofByteArray arr)
  (A.thawByteArray arr s l)

copyByteArray :: forall m. (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyByteArray marr s1 arr s2 l = do
  let siz = A.sizeofMutableByteArray marr
  check "copyByteArray: index range of out bounds"
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz && s2 + l <= A.sizeofByteArray arr)
    (A.copyByteArray marr s1 arr s2 l)

copyMutableByteArray :: forall m. (HasCallStack, PrimMonad m)
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
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz1 && s2 + l <= siz2)
    (A.copyMutableByteArray marr1 s1 marr2 s2 l)

copyByteArrayToPtr :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyByteArrayToPtr ptr arr s l = do
  let srcSz = elementSizeofByteArray (Proxy :: Proxy a) arr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyByteArrayToPtr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyByteArrayToPtr ptr arr s l)

copyMutableByteArrayToPtr :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutableByteArrayToPtr ptr marr s l = do
  srcSz <- getElementSizeofMutableByteArray (Proxy :: Proxy a) marr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyMutableByteArrayToPtr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyMutableByteArrayToPtr ptr marr s l)

copyByteArrayToAddr :: (HasCallStack, PrimMonad m)
  => Ptr Word8 -- ^ destination pointer
  -> ByteArray -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyByteArrayToAddr ptr arr s l = do
  let srcSz = A.sizeofByteArray arr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyByteArrayToAddr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyByteArrayToAddr ptr arr s l)

copyMutableByteArrayToAddr :: (HasCallStack, PrimMonad m)
  => Ptr Word8 -- ^ destination pointer
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyMutableByteArrayToAddr ptr marr s l = do
  srcSz <- A.getSizeofMutableByteArray marr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyMutableByteArrayToAddr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyMutableByteArrayToAddr ptr marr s l)

moveByteArray :: forall m. (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutableByteArray (PrimState m) -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
moveByteArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofMutableByteArray marr1
  let siz2 = A.sizeofMutableByteArray marr2
  check "moveByteArray: index range of out bounds"
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= siz1 && s2 + l <= siz2)
    (A.moveByteArray marr1 s1 marr2 s2 l)

fillByteArray :: (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of bytes to fill
  -> Word8 -- ^ byte to fill with
  -> m ()
fillByteArray = setByteArray

setByteArray :: forall m a. (HasCallStack, Prim a, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ array to fill
  -> Int -- ^ offset into array
  -> Int -- ^ number of values to fill
  -> a -- ^ value to fill with
  -> m ()
setByteArray dst doff sz x = do
  siz <- getElementSizeofMutableByteArray (Proxy :: Proxy a) dst
  check "setByteArray: index range of out bounds"
    (doff >= 0 && doff + sz <= siz)
    (A.setByteArray dst doff sz x)

cloneByteArray :: HasCallStack
  => ByteArray -- ^ source array
  -> Int       -- ^ offset into source array
  -> Int       -- ^ number of bytes to copy
  -> ByteArray
cloneByteArray arr s l = check "cloneByteArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofByteArray arr)
  (A.cloneByteArray arr s l)

cloneMutableByteArray :: (HasCallStack, PrimMonad m)
  => MutableByteArray (PrimState m) -- ^ source array
  -> Int                            -- ^ offset into source array
  -> Int                            -- ^ number of bytes to copy
  -> m (MutableByteArray (PrimState m))
cloneMutableByteArray marr s l = check "cloneMutableByteArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutableByteArray marr)
  (A.cloneMutableByteArray marr s l)
