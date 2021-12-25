{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Primitive.PrimArray
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
  , newPinnedPrimArray
  , newAlignedPinnedPrimArray
  , resizeMutablePrimArray
  , shrinkMutablePrimArray
    -- * Element Access
  , readPrimArray
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , freezePrimArray
  , thawPrimArray
  , unsafeFreezePrimArray
  , A.unsafeThawPrimArray
    -- * Block Operations
  , copyPrimArray
  , copyMutablePrimArray
  , copyPrimArrayToPtr
  , copyMutablePrimArrayToPtr
  , clonePrimArray
  , cloneMutablePrimArray
  , setPrimArray
    -- * Information
  , A.sameMutablePrimArray
  , A.getSizeofMutablePrimArray
  , A.sizeofMutablePrimArray
  , A.sizeofPrimArray
  , A.primArrayContents
  , A.mutablePrimArrayContents
  , A.isPrimArrayPinned
  , A.isMutablePrimArrayPinned
    -- * List Conversion
  , A.primArrayToList
  , A.primArrayFromList
  , A.primArrayFromListN
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

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Exception (throw, ArrayException(..))
import Data.Primitive.Types (Prim, Ptr, sizeOf)
import Data.Word (Word8)
import "primitive" Data.Primitive.PrimArray (PrimArray, MutablePrimArray)
import qualified "primitive" Data.Primitive.PrimArray as A
import GHC.Stack
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.PrimArray." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

newPrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray n =
    check "newPrimArray: negative size" (n >= 0)
  $ check ("newPrimArray: requested " ++ show n ++ " elements of size " ++ show elemSz) (n * elemSz < 1024*1024*1024)
  $ A.newPrimArray n
  where
  elemSz = sizeOf (undefined :: a)

newPinnedPrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPinnedPrimArray n =
    check "newPinnedPrimArray: negative size" (n >= 0)
  $ check ("newPinnedPrimArray: requested " ++ show n ++ " elements of size " ++ show elemSz) (n * elemSz < 1024*1024*1024)
  $ A.newPinnedPrimArray n
  where
  elemSz = sizeOf (undefined :: a)

newAlignedPinnedPrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newAlignedPinnedPrimArray n =
    check "newAlignedPinnedPrimArray: negative size" (n >= 0)
  $ check ("newAlignedPinnedPrimArray: requested " ++ show n ++ " elements of size " ++ show elemSz) (n * elemSz < 1024*1024*1024)
  $ A.newAlignedPinnedPrimArray n
  where
  elemSz = sizeOf (undefined :: a)

-- | After a call to resizeMutablePrimArray, the original reference to
-- the mutable array should not be used again. This cannot truly be enforced
-- except by linear types. To attempt to enforce this, we always make a
-- copy of the mutable primitive array and intentionally corrupt the original
-- of the original one. The strategy used here to corrupt the array is
-- simply to write 1 to every bit.
resizeMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m (MutablePrimArray (PrimState m) a)
resizeMutablePrimArray marr@(A.MutablePrimArray x) n = check "resizeMutablePrimArray: negative size" (n >= 0) $ do
  let sz = A.sizeofMutablePrimArray marr
  marr' <- A.cloneMutablePrimArray marr 0 sz
  A.setPrimArray (A.MutablePrimArray x) 0 (sz * sizeOf (undefined :: a)) (0xFF :: Word8)
  return marr'

freezePrimArray
  :: (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ source
  -> Int                              -- ^ offset
  -> Int                              -- ^ length
  -> m (PrimArray a)
freezePrimArray marr s l = check "freezePrimArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutablePrimArray marr)
  (A.freezePrimArray marr s l)

thawPrimArray
  :: (HasCallStack, PrimMonad m, Prim a)
  => PrimArray a -- ^ source
  -> Int         -- ^ offset
  -> Int         -- ^ length
  -> m (MutablePrimArray (PrimState m) a)
thawPrimArray arr s l = check "thawPrimArray: index range of out bounds"
    (s >= 0 && l >= 0 && s + l <= A.sizeofPrimArray arr)
    (A.thawPrimArray arr s l)

-- | This corrupts the contents of the argument array.
unsafeFreezePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> m (PrimArray a)
unsafeFreezePrimArray marr@(A.MutablePrimArray x) = do
  let sz = A.sizeofMutablePrimArray marr
  arr <- A.freezePrimArray marr 0 sz
  A.setPrimArray (A.MutablePrimArray x) 0 (sz * sizeOf (undefined :: a)) (0xFF :: Word8)
  return arr

shrinkMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m ()
shrinkMutablePrimArray marr n = do
  old <- A.getSizeofMutablePrimArray marr
  check "shrinkMutablePrimArray: illegal new size" (n >= 0 && n <= old) (A.shrinkMutablePrimArray marr n)

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
  check ("readPrimArray: index out of bounds " ++ explain) (i >= 0 && i < siz) (A.readPrimArray marr i)

writePrimArray :: (HasCallStack, Prim a, PrimMonad m)
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
  check ("writePrimArray: index out of bounds " ++ explain) (i >= 0 && i < siz) (A.writePrimArray marr i x)

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
   in check ("indexPrimArray: index out of bounds " ++ explain)
        (i >= 0 && i < sz)
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
    (doff >= 0 && doff + sz <= arrSz)
    (A.setPrimArray dst doff sz x)

copyPrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
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
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= dstSz && s2 + l <= srcSz)
    (A.copyPrimArray marr s1 arr s2 l)

copyMutablePrimArray :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutablePrimArray marr1 s1 marr2 s2 l = do
  dstSz <- A.getSizeofMutablePrimArray marr1
  srcSz <- A.getSizeofMutablePrimArray marr2
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
  check ("copyMutablePrimArray: index range of out bounds " ++ explain)
    (s1 >= 0 && s2 >= 0 && l >= 0 && s1 + l <= dstSz && s2 + l <= srcSz)
    (A.copyMutablePrimArray marr1 s1 marr2 s2 l)

copyPrimArrayToPtr :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyPrimArrayToPtr ptr arr s l = do
  let srcSz = A.sizeofPrimArray arr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyPrimArrayToPtr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyPrimArrayToPtr ptr arr s l)

copyMutablePrimArrayToPtr :: forall m a. (HasCallStack, PrimMonad m, Prim a)
  => Ptr a -- ^ destination pointer
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutablePrimArrayToPtr ptr marr s l = do
  srcSz <- A.getSizeofMutablePrimArray marr
  let explain = L.concat
        [ "[src_sz: "
        , show srcSz
        , ", src_off: "
        , show s
        , ", len: "
        , show l
        , "]"
        ]
  check ("copyMutablePrimArrayToPtr: index range of out bounds " ++ explain)
    (s >= 0 && l >= 0 && s + l <= srcSz)
    (A.copyMutablePrimArrayToPtr ptr marr s l)

clonePrimArray :: (HasCallStack, Prim a)
  => PrimArray a -- ^ source array
  -> Int         -- ^ offset into source array
  -> Int         -- ^ number of elements to copy
  -> PrimArray a
clonePrimArray arr s l = check "clonePrimArray: index range of out bounds"
    (s >= 0 && l >= 0 && s + l <= A.sizeofPrimArray arr)
    (A.clonePrimArray arr s l)

cloneMutablePrimArray :: (HasCallStack, PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ source array
  -> Int                              -- ^ offset into source array
  -> Int                              -- ^ number of elements to copy
  -> m (MutablePrimArray (PrimState m) a)
cloneMutablePrimArray marr s l = check "cloneMutablePrimArray: index range of out bounds"
  (s >= 0 && l >= 0 && s + l <= A.sizeofMutablePrimArray marr)
  (A.cloneMutablePrimArray marr s l)
