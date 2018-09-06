{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , indexSmallArray
  , indexSmallArray##
  , indexSmallArrayM
  , freezeSmallArray
  , thawSmallArray
  , A.unsafeFreezeSmallArray
  , A.unsafeThawSmallArray
  , copySmallArray
  , copySmallMutableArray
  , cloneSmallArray
  , cloneSmallMutableArray
  , A.sizeofSmallArray
  , A.sizeofSmallMutableArray
  , A.smallArrayFromList
  , A.smallArrayFromListN
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Control.Exception (throw, ArrayException(..), Exception, toException)
import "primitive" Data.Primitive.SmallArray (SmallArray,SmallMutableArray)
import qualified "primitive" Data.Primitive.SmallArray as A
import GHC.Exts (raise#)
import GHC.Stack
import qualified Data.List as L

check :: HasCallStack => String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (IndexOutOfBounds $ "Data.Primitive.SmallArray.Checked." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

checkUnary :: HasCallStack => String -> Bool -> (# a #) -> (# a #)
checkUnary _      True  x = x
checkUnary errMsg False _ = throwUnary (IndexOutOfBounds $ "Data.Primitive.SmallArray.Checked." ++ errMsg ++ "\n" ++ prettyCallStack callStack)

throwUnary :: Exception e => e -> (# a #)
throwUnary e = raise# (toException e)

newSmallArray :: (HasCallStack, PrimMonad m) => Int -> a -> m (SmallMutableArray (PrimState m) a)
newSmallArray n x = check "newSmallArray: negative size" (n>=0) (A.newSmallArray n x)

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
  check ("readSmallArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.readSmallArray marr i)

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
  check ("writeSmallArray: index of out bounds " ++ explain) (i>=0 && i<siz) (A.writeSmallArray marr i x)

indexSmallArray :: HasCallStack => SmallArray a -> Int -> a
indexSmallArray arr i = check ("indexSmallArray: index of out bounds " ++ explain)
  (i>=0 && i<A.sizeofSmallArray arr)
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
indexSmallArray## arr i = checkUnary "indexSmallArray##: index of out bounds"
  (i>=0 && i<A.sizeofSmallArray arr)
  (A.indexSmallArray## arr i)

indexSmallArrayM :: (HasCallStack, Monad m) => SmallArray a -> Int -> m a
indexSmallArrayM arr i = check "indexSmallArrayM: index of out bounds"
    (i>=0 && i<A.sizeofSmallArray arr)
    (A.indexSmallArrayM arr i)

freezeSmallArray
  :: (HasCallStack, PrimMonad m)
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                          -- ^ offset
  -> Int                          -- ^ length
  -> m (SmallArray a)
freezeSmallArray marr s l = do
  let siz = A.sizeofSmallMutableArray marr
  check "freezeSmallArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (A.freezeSmallArray marr s l)

thawSmallArray
  :: (HasCallStack, PrimMonad m)
  => SmallArray a -- ^ source
  -> Int     -- ^ offset
  -> Int     -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
thawSmallArray arr s l = check "thawArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofSmallArray arr)
    (A.thawSmallArray arr s l)

copySmallArray :: (HasCallStack, PrimMonad m)
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallArray a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
copySmallArray marr s1 arr s2 l = do
  let siz = A.sizeofSmallMutableArray marr
  check "copySmallArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofSmallArray arr && (s1+l)<=siz)
    (A.copySmallArray marr s1 arr s2 l)


copySmallMutableArray :: (HasCallStack, PrimMonad m)
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallMutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
copySmallMutableArray marr1 s1 marr2 s2 l = do
  let siz1 = A.sizeofSmallMutableArray marr1
  let siz2 = A.sizeofSmallMutableArray marr2
  check "copySmallMutableArray: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=siz2 && (s1+l)<=siz1)
    (A.copySmallMutableArray marr1 s1 marr2 s2 l)


cloneSmallArray :: HasCallStack
           => SmallArray a -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> SmallArray a
cloneSmallArray arr s l = check "cloneSmallArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofSmallArray arr)
    (A.cloneSmallArray arr s l)

cloneSmallMutableArray :: (HasCallStack, PrimMonad m)
        => SmallMutableArray (PrimState m) a -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (SmallMutableArray (PrimState m) a)
cloneSmallMutableArray marr s l = do
  let siz = A.sizeofSmallMutableArray marr
  check "cloneSmallMutableArray: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=siz)
    (A.cloneSmallMutableArray marr s l)
