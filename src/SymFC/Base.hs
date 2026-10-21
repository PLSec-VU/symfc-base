{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SymFC.Base
  ( embeddings
  ) where

import Control.Exception.Base qualified as GHC (patError, throw)
import Data.Constraint.Unsafe (unsafeSNat)
import Data.Functor.Identity (Identity (..))
import Data.List qualified as GHC (zip)
import GHC.Base
  ( TYPE
  , Void
  , Int#
  , Int8#
  , Int16#
  , Int32#
  , Int64#
  , Word#
  , Word8#
  , Word16#
  , Word32#
  , Word64#
  , Addr#
  , RuntimeRep (..)
  , State#
  , RealWorld
  , coerce
  )
import GHC.Base qualified as GHC
import GHC.Int (Int (..), Int8 (..), Int16 (..), Int32 (..), Int64 (..))
import GHC.Num (Integer (..), Natural (..))
import GHC.Num qualified as GHC
  ( integerFromBigNat#
  , integerFromBigNatNeg#
  , integerFromNatural
  , integerFromWord#
  , integerSub
  , integerToInt#
  , integerToNatural
  , integerToNaturalThrow
  , integerToWord#
  , naturalAdd
  , naturalSubThrow
  , naturalFromBigNat#
  , naturalFromWord#
  )
import GHC.Num.Primitives qualified as GHC (wordFromAbsInt#)
import GHC.Num.BigNat qualified as GHC
  ( bigNatFromWord#
  , bigNatToWord#
  , bigNatAddWord#
  , bigNatAdd
  , bigNatCompare
  , bigNatFromWord2#
  , bigNatSubWordUnsafe#
  , bigNatSub
  , bigNatSubUnsafe
  )
import GHC.Prim qualified as GHC
import GHC.Prim.Exception qualified as GHC
import GHC.Prim.Panic qualified as GHC
import GHC.TypeLits (KnownNat, SNat, type (+))
import GHC.TypeNats qualified as GHC (withSomeSNat)
import GHC.Word (Word (..), Word8 (..), Word16 (..), Word32 (..), Word64 (..))
import SymFC (Embeddings (..))
import SymFC.BuiltIn qualified as SymFC
import SymFC.BuiltIn
  ( Embeddable
  , Embedding (..)
  , BitVec
  , PlatformWordSize
  , Symbolic (..)
  , embedding
  )
import Prelude hiding (undefined, map, zip, fromInteger, toInteger)
import Unsafe.Coerce (unsafeCoerce)

embeddings :: Embeddings
embeddings = Embeddings
  { typeEmbeddings =
    [ (''Int#, ''BitVecPW)
    , (''Int8#, ''BitVec8)
    , (''Int16#, ''BitVec16)
    , (''Int32#, ''BitVec32)
    , (''Int64#, ''BitVec64)
    , (''Word#, ''BitVecPW)
    , (''Word8#, ''BitVec8)
    , (''Word16#, ''BitVec16)
    , (''Word32#, ''BitVec32)
    , (''Word64#, ''BitVec64)
    ]
  , termEmbeddings =
    -- SymFC embed operations.
    ------------------------------
    [ ('SymFC.toInt#, 'fromBV)
    , ('SymFC.toInt8#, 'fromBV)
    , ('SymFC.toInt16#, 'fromBV)
    , ('SymFC.toInt32#, 'fromBV)
    , ('SymFC.toInt64#, 'fromBV)
    , ('SymFC.toWord#, 'fromBV)
    , ('SymFC.toWord8#, 'fromBV)
    , ('SymFC.toWord16#, 'fromBV)
    , ('SymFC.toWord32#, 'fromBV)
    , ('SymFC.toWord64#, 'fromBV)
    , ('SymFC.fromInt#, 'toBV)
    , ('SymFC.fromInt8#, 'toBV)
    , ('SymFC.fromInt16#, 'toBV)
    , ('SymFC.fromInt32#, 'toBV)
    , ('SymFC.fromInt64#, 'toBV)
    , ('SymFC.fromWord#, 'toBV)
    , ('SymFC.fromWord8#, 'toBV)
    , ('SymFC.fromWord16#, 'toBV)
    , ('SymFC.fromWord32#, 'toBV)
    , ('SymFC.fromWord64#, 'toBV)

    -- Integer to symbolic primitive conversions.
    ----------------------------------------------
    , ('SymFC.fromInteger, 'fromInteger)
    , ('SymFC.toInteger, 'toInteger)

    -- System FC primitive operations.
    ----------------------------------
    , ('SymFC.tagToEnum#, 'tagToEnum)
    -- TODO: While these might be deprecated for use, they are in fact required
    -- in this context... Not sure what to do if these are unexposed (but still
    -- used internally) in the future.
    , ('GHC.dataToTagSmall#, 'dataToTag)
    , ('GHC.dataToTagLarge#, 'dataToTag)
    , ('GHC.raise#, 'SymFC.raise)

    -- Int# primitive operations.
    -----------------------------
    , ('GHC.intToInt8#, 'intToInt8#)
    , ('GHC.intToInt16#, 'intToInt16#)
    , ('GHC.intToInt32#, 'intToInt32#)
    , ('GHC.intToInt64#, 'intToInt64#)
    , ('GHC.int2Word#, 'int2Word#)
    -- , ('GHC.int2Float#, 'undefined)
    -- , ('GHC.int2Double#, 'undefined)
    , ('(GHC.+#), '(+#))
    , ('(GHC.-#), '(-#))
    , ('(GHC.*#), '(*#))
    , ('GHC.addIntC#, 'addIntC#)
    , ('GHC.subIntC#, 'subIntC#)
    -- , ('GHC.timesInt2#, 'timesInt2#)
    -- , ('GHC.mulIntMayOflo#, 'mulIntMayOflo#)
    -- , ('GHC.quotInt#, 'quotInt#)
    -- , ('GHC.remInt#, 'remInt#)
    -- , ('GHC.quotRemInt#, 'quotRemInt#)
    , ('GHC.andI#, 'andI#)
    , ('GHC.orI#, 'orI#)
    , ('GHC.xorI#, 'xorI#)
    , ('GHC.notI#, 'notI#)
    , ('GHC.negateInt#, 'negateInt#)
    -- , ('GHC.uncheckedIShiftL#, 'uncheckedIShiftL#)
    -- , ('GHC.uncheckedIShiftRA#, 'uncheckedIShiftRA#)
    -- , ('GHC.uncheckedIShiftRL#, 'uncheckedIShiftRL#)
    , ('(GHC.==#), '(==#))
    , ('(GHC./=#), '(/=#))
    , ('(GHC.>=#), '(>=#))
    , ('(GHC.>#), '(>#))
    , ('(GHC.<=#), '(<=#))
    , ('(GHC.<#), '(<#))

    -- Int8# primitive operations.
    ------------------------------
    , ('GHC.int8ToInt#, 'int8ToInt#)
    , ('GHC.int8ToWord8#, 'int8ToWord8#)
    , ('GHC.plusInt8#, 'plusInt8#)
    , ('GHC.subInt8#, 'subInt8#)
    , ('GHC.timesInt8#, 'timesInt8#)
    -- , ('GHC.quotInt8#, 'quotInt8#)
    -- , ('GHC.remInt8#, 'remInt8#)
    -- , ('GHC.quotRemInt8#, 'quotRemInt8#)
    -- , ('GHC.uncheckedShiftLInt8#, 'uncheckedShiftLInt8#)
    -- , ('GHC.uncheckedShiftRAInt8#, 'uncheckedShiftRAInt8#)
    -- , ('GHC.uncheckedShiftRLInt8#, 'uncheckedShiftRLInt8#)
    , ('GHC.negateInt8#, 'negateInt8#)
    , ('GHC.eqInt8#, 'eqInt8#)
    , ('GHC.neInt8#, 'neInt8#)
    , ('GHC.geInt8#, 'geInt8#)
    , ('GHC.gtInt8#, 'gtInt8#)
    , ('GHC.leInt8#, 'leInt8#)
    , ('GHC.ltInt8#, 'ltInt8#)

    -- Int16# primitive operations.
    ------------------------------
    , ('GHC.int16ToInt#, 'int16ToInt#)
    , ('GHC.int16ToWord16#, 'int16ToWord16#)
    , ('GHC.plusInt16#, 'plusInt16#)
    , ('GHC.subInt16#, 'subInt16#)
    , ('GHC.timesInt16#, 'timesInt16#)
    -- , ('GHC.quotInt16#, 'quotInt16#)
    -- , ('GHC.remInt16#, 'remInt16#)
    -- , ('GHC.quotRemInt16#, 'quotRemInt16#)
    -- , ('GHC.uncheckedShiftLInt16#, 'uncheckedShiftLInt16#)
    -- , ('GHC.uncheckedShiftRAInt16#, 'uncheckedShiftRAInt16#)
    -- , ('GHC.uncheckedShiftRLInt16#, 'uncheckedShiftRLInt16#)
    , ('GHC.negateInt16#, 'negateInt16#)
    , ('GHC.eqInt16#, 'eqInt16#)
    , ('GHC.neInt16#, 'neInt16#)
    , ('GHC.geInt16#, 'geInt16#)
    , ('GHC.gtInt16#, 'gtInt16#)
    , ('GHC.leInt16#, 'leInt16#)
    , ('GHC.ltInt16#, 'ltInt16#)

    -- Int32# primitive operations.
    ------------------------------
    , ('GHC.int32ToInt#, 'int32ToInt#)
    , ('GHC.int32ToWord32#, 'int32ToWord32#)
    , ('GHC.plusInt32#, 'plusInt32#)
    , ('GHC.subInt32#, 'subInt32#)
    , ('GHC.timesInt32#, 'timesInt32#)
    -- , ('GHC.quotInt32#, 'quotInt32#)
    -- , ('GHC.remInt32#, 'remInt32#)
    -- , ('GHC.quotRemInt32#, 'quotRemInt32#)
    -- , ('GHC.uncheckedShiftLInt32#, 'uncheckedShiftLInt32#)
    -- , ('GHC.uncheckedShiftRAInt32#, 'uncheckedShiftRAInt32#)
    -- , ('GHC.uncheckedShiftRLInt32#, 'uncheckedShiftRLInt32#)
    , ('GHC.negateInt32#, 'negateInt32#)
    , ('GHC.eqInt32#, 'eqInt32#)
    , ('GHC.neInt32#, 'neInt32#)
    , ('GHC.geInt32#, 'geInt32#)
    , ('GHC.gtInt32#, 'gtInt32#)
    , ('GHC.leInt32#, 'leInt32#)
    , ('GHC.ltInt32#, 'ltInt32#)

    -- Int64# primitive operations.
    ------------------------------
    , ('GHC.int64ToInt#, 'int64ToInt#)
    , ('GHC.int64ToWord64#, 'int64ToWord64#)
    , ('GHC.plusInt64#, 'plusInt64#)
    , ('GHC.subInt64#, 'subInt64#)
    , ('GHC.timesInt64#, 'timesInt64#)
    -- , ('GHC.quotInt64#, 'quotInt64#)
    -- , ('GHC.remInt64#, 'remInt64#)
    -- , ('GHC.uncheckedIShiftL64#, 'uncheckedIShiftL64#)
    -- , ('GHC.uncheckedIShiftRA64#, 'uncheckedIShiftRA64#)
    -- , ('GHC.uncheckedIShiftRL64#, 'uncheckedIShiftRL64#)
    , ('GHC.negateInt64#, 'negateInt64#)
    , ('GHC.eqInt64#, 'eqInt64#)
    , ('GHC.neInt64#, 'neInt64#)
    , ('GHC.geInt64#, 'geInt64#)
    , ('GHC.gtInt64#, 'gtInt64#)
    , ('GHC.leInt64#, 'leInt64#)
    , ('GHC.ltInt64#, 'ltInt64#)

    -- Word# primitive operations.
    ------------------------------
    , ('GHC.wordToWord8#, 'wordToWord8#)
    , ('GHC.wordToWord16#, 'wordToWord16#)
    , ('GHC.wordToWord32#, 'wordToWord32#)
    , ('GHC.wordToWord64#, 'wordToWord64#)
    , ('GHC.word2Int#, 'word2Int#)
    -- , ('GHC.word2Float#, 'word2Float#)
    -- , ('GHC.word2Double#, 'word2Double#)
    , ('GHC.plusWord#, 'plusWord#)
    , ('GHC.minusWord#, 'minusWord#)
    , ('GHC.timesWord#, 'timesWord#)
    , ('GHC.addWordC#, 'addWordC#)
    , ('GHC.subWordC#, 'subWordC#)
    -- , ('GHC.plusWord2#, 'plusWord2#)
    -- , ('GHC.timesWord2#, 'timesWord2#)
    -- , ('GHC.quotWord#, 'quotWord#)
    -- , ('GHC.remWord#, 'remWord#)
    -- , ('GHC.quotRemWord#, 'quotRemWord#)
    -- , ('GHC.quotRemWord2#, 'quotRemWord2#)
    , ('GHC.and#, 'and#)
    , ('GHC.or#, 'or#)
    , ('GHC.xor#, 'xor#)
    , ('GHC.not#, 'not#)
    , ('GHC.uncheckedShiftL#, 'uncheckedShiftL#)
    , ('GHC.uncheckedShiftRL#, 'uncheckedShiftRL#)
    , ('GHC.eqWord#, 'eqWord#)
    , ('GHC.neWord#, 'neWord#)
    , ('GHC.geWord#, 'geWord#)
    , ('GHC.gtWord#, 'gtWord#)
    , ('GHC.leWord#, 'leWord#)
    , ('GHC.ltWord#, 'ltWord#)

    -- Word8# primitive operations.
    ------------------------------
    , ('GHC.word8ToWord#, 'word8ToWord#)
    , ('GHC.word8ToInt8#, 'word8ToInt8#)
    , ('GHC.plusWord8#, 'plusWord8#)
    , ('GHC.subWord8#, 'subWord8#)
    , ('GHC.timesWord8#, 'timesWord8#)
    -- , ('GHC.quotWord8#, 'quotWord8#)
    -- , ('GHC.remWord8#, 'remWord8#)
    -- , ('GHC.quotRemWord8#, 'quotRemWord8#)
    , ('GHC.andWord8#, 'andWord8#)
    , ('GHC.orWord8#, 'orWord8#)
    , ('GHC.xorWord8#, 'xorWord8#)
    , ('GHC.notWord8#, 'notWord8#)
    -- , ('GHC.uncheckedShiftLWord8#, 'uncheckedShiftLWord8#)
    -- , ('GHC.uncheckedShiftRLWord8#, 'uncheckedShiftRLWord8#)
    , ('GHC.eqWord8#, 'eqWord8#)
    , ('GHC.neWord8#, 'neWord8#)
    , ('GHC.geWord8#, 'geWord8#)
    , ('GHC.gtWord8#, 'gtWord8#)
    , ('GHC.leWord8#, 'leWord8#)
    , ('GHC.ltWord8#, 'ltWord8#)

    -- Word16# primitive operations.
    ------------------------------
    , ('GHC.word16ToWord#, 'word16ToWord#)
    , ('GHC.word16ToInt16#, 'word16ToInt16#)
    , ('GHC.plusWord16#, 'plusWord16#)
    , ('GHC.subWord16#, 'subWord16#)
    , ('GHC.timesWord16#, 'timesWord16#)
    -- , ('GHC.quotWord16#, 'quotWord16#)
    -- , ('GHC.remWord16#, 'remWord16#)
    -- , ('GHC.quotRemWord16#, 'quotRemWord16#)
    , ('GHC.andWord16#, 'andWord16#)
    , ('GHC.orWord16#, 'orWord16#)
    , ('GHC.xorWord16#, 'xorWord16#)
    , ('GHC.notWord16#, 'notWord16#)
    -- , ('GHC.uncheckedShiftLWord16#, 'uncheckedShiftLWord16#)
    -- , ('GHC.uncheckedShiftRLWord16#, 'uncheckedShiftRLWord16#)
    , ('GHC.eqWord16#, 'eqWord16#)
    , ('GHC.neWord16#, 'neWord16#)
    , ('GHC.geWord16#, 'geWord16#)
    , ('GHC.gtWord16#, 'gtWord16#)
    , ('GHC.leWord16#, 'leWord16#)
    , ('GHC.ltWord16#, 'ltWord16#)

    -- Word32# primitive operations.
    ------------------------------
    , ('GHC.word32ToWord#, 'word32ToWord#)
    , ('GHC.word32ToInt32#, 'word32ToInt32#)
    , ('GHC.plusWord32#, 'plusWord32#)
    , ('GHC.subWord32#, 'subWord32#)
    , ('GHC.timesWord32#, 'timesWord32#)
    -- , ('GHC.quotWord32#, 'quotWord32#)
    -- , ('GHC.remWord32#, 'remWord32#)
    -- , ('GHC.quotRemWord32#, 'quotRemWord32#)
    , ('GHC.andWord32#, 'andWord32#)
    , ('GHC.orWord32#, 'orWord32#)
    , ('GHC.xorWord32#, 'xorWord32#)
    , ('GHC.notWord32#, 'notWord32#)
    -- , ('GHC.uncheckedShiftLWord32#, 'uncheckedShiftLWord32#)
    -- , ('GHC.uncheckedShiftRLWord32#, 'uncheckedShiftRLWord32#)
    , ('GHC.eqWord32#, 'eqWord32#)
    , ('GHC.neWord32#, 'neWord32#)
    , ('GHC.geWord32#, 'geWord32#)
    , ('GHC.gtWord32#, 'gtWord32#)
    , ('GHC.leWord32#, 'leWord32#)
    , ('GHC.ltWord32#, 'ltWord32#)

    -- Word64# primitive operations.
    ------------------------------
    , ('GHC.word64ToWord#, 'word64ToWord#)
    , ('GHC.word64ToInt64#, 'word64ToInt64#)
    , ('GHC.plusWord64#, 'plusWord64#)
    , ('GHC.subWord64#, 'subWord64#)
    , ('GHC.timesWord64#, 'timesWord64#)
    -- , ('GHC.quotWord64#, 'quotWord64#)
    -- , ('GHC.remWord64#, 'remWord64#)
    , ('GHC.and64#, 'and64#)
    , ('GHC.or64#, 'or64#)
    , ('GHC.xor64#, 'xor64#)
    , ('GHC.not64#, 'not64#)
    -- , ('GHC.uncheckedShiftL64#, 'uncheckedShiftL64#)
    -- , ('GHC.uncheckedShiftRL64#, 'uncheckedShiftRL64#)
    , ('GHC.eqWord64#, 'eqWord64#)
    , ('GHC.neWord64#, 'neWord64#)
    , ('GHC.geWord64#, 'geWord64#)
    , ('GHC.gtWord64#, 'gtWord64#)
    , ('GHC.leWord64#, 'leWord64#)
    , ('GHC.ltWord64#, 'ltWord64#)

    -- Haskell functions without unfoldings.
    ----------------------------------------
    -- NOTE: Ideally we would not have these. It's just that GHC tosses
    -- their unfolding and we cannot get 'base' to be compiled with the flag
    -- 'expose-all-unfoldings' as 'base' is tied to the compiler...
    , ('GHC.integerFromWord#, 'integerFromWord#)
    , ('GHC.integerToWord#, 'integerToWord#)
    , ('GHC.integerFromNatural, 'integerFromNatural)
    , ('GHC.integerToNatural, 'integerToNatural)
    , ('GHC.integerToNaturalThrow, 'integerToNaturalThrow)
    , ('GHC.integerToInt#, 'integerToInt#)
    , ('GHC.integerSub, 'integerSub)
    , ('GHC.naturalAdd, 'naturalAdd)
    , ('GHC.naturalSubThrow, 'naturalSubThrow)
    , ('GHC.noinline, 'noinline)
    , ('GHC.undefined, 'undefined)
    , ('GHC.throw, 'throw)
    , ('GHC.patError, 'patError)
    , ('GHC.absentError, 'absentError)
    , ('GHC.withSomeSNat, 'withSomeSNat)
    , ('GHC.map, 'map)
    , ('GHC.zip, 'zip)
    , ('GHC.runRW#, 'runRW#)
    ]
  }

type BitVecPW = BitVec PlatformWordSize

type BitVec8 = BitVec 8

type BitVec16 = BitVec 16

type BitVec32 = BitVec 32

type BitVec64 = BitVec 64

fromBV
  :: forall r n (a :: TYPE r)
   . Embeddable a (BitVec n)
  => BitVec n
  -> a
fromBV | Embedding <- embedding @a = coerce

toBV
  :: forall r n (a :: TYPE r)
   . Embeddable a (BitVec n)
  => a
  -> BitVec n
toBV | Embedding <- embedding @a = coerce

bool2I# :: SymFC.Bool -> Int#
bool2I# scrut = SymFC.iteIP scrut 1# 0#

tagToEnum :: SymFC.EnumCon a => Int# -> a
tagToEnum i = SymFC.tagToEnum $ SymFC.fromInt# i

dataToTag :: forall l (a :: TYPE (BoxedRep l)). a -> Int#
-- SAFETY: We need some 'unsafeCoerce' magic because we cannot bind a type
-- without a fixed runtime representation. The symbolic evaluator doesn't care
-- at all about 'RuntimeRep' and so we can just fake it to be 'Lifted'.
-- Perhaps once embed becomes more like coerce that we can do the following:
-- dataToTag = SymFC.embed SymFC.dataToTag
dataToTag = unsafeCoerce @(forall b. b -> Int#) @(forall b. b -> Int#) go
  where
    go :: forall b. b -> Int#
    go x = SymFC.toInt# $ SymFC.dataToTag x

int2Word# :: Int# -> Word#
int2Word# x = SymFC.toWord# $ SymFC.fromInt# x

intToInt8# :: Int# -> Int8#
intToInt8# x = SymFC.toInt8# $ SymFC.bvselect @0 $ SymFC.fromInt# x

intToInt16# :: Int# -> Int16#
intToInt16# x = SymFC.toInt16# $ SymFC.bvselect @0 $ SymFC.fromInt# x

intToInt32# :: Int# -> Int32#
intToInt32# x = SymFC.toInt32# $ SymFC.bvselect @0 $ SymFC.fromInt# x

intToInt64# :: Int# -> Int64#
intToInt64# x = SymFC.toInt64# $ SymFC.bvselect @0 $ SymFC.fromInt# x

binaryInt#
  :: (BitVecPW -> BitVecPW -> BitVecPW)
  -> Int#
  -> Int#
  -> Int#
binaryInt# f lhs rhs = do
  let lhs' = SymFC.fromInt# lhs
  let rhs' = SymFC.fromInt# rhs
  SymFC.toInt# $ f lhs' rhs'

(+#) :: Int# -> Int# -> Int#
(+#) = binaryInt# (+)

(-#) :: Int# -> Int# -> Int#
(-#) = binaryInt# (-)

(*#) :: Int# -> Int# -> Int#
(*#) = binaryInt# (*)

binaryIntC#
  :: (forall n. KnownNat n => BitVec n -> BitVec n -> BitVec n)
  -> Int#
  -> Int#
  -> (# Int#, Int# #)
binaryIntC# f lhs rhs = do
  let project' x
        = SymFC.bvzext @_ @(PlatformWordSize + 1)
        $ SymFC.fromInt# x
  let lhs' = project' lhs
  let rhs' = project' rhs

  let result = f lhs' rhs'
  let add = SymFC.bvselect @0 result
  let carry = SymFC.bvselect @PlatformWordSize @1 result
  let carry' = SymFC.bvzext carry
  (# SymFC.toInt# add, SymFC.toInt# carry' #)

addIntC# :: Int# -> Int# -> (# Int#, Int# #)
addIntC# = binaryIntC# SymFC.bvadd

subIntC# :: Int# -> Int# -> (# Int#, Int# #)
subIntC# = binaryIntC# \lhs rhs -> SymFC.bvadd lhs (SymFC.bvneg rhs)

andI# :: Int# -> Int# -> Int#
andI# = binaryInt# SymFC.bvand

orI# :: Int# -> Int# -> Int#
orI# = binaryInt# SymFC.bvor

xorI# :: Int# -> Int# -> Int#
xorI# = binaryInt# SymFC.bvxor

unaryInt#
  :: (BitVecPW -> BitVecPW)
  -> Int#
  -> Int#
unaryInt# f x = do
  let x' = SymFC.fromInt# x
  SymFC.toInt# $ f x'

notI# :: Int# -> Int#
notI# = unaryInt# SymFC.bvnot

negateInt# :: Int# -> Int#
negateInt# = unaryInt# SymFC.bvneg

compareInt#
  :: (BitVecPW -> BitVecPW -> SymFC.Bool)
  -> Int#
  -> Int#
  -> Int#
compareInt# f lhs rhs = do
  let lhs' = SymFC.fromInt# lhs
  let rhs' = SymFC.fromInt# rhs
  bool2I# $ f lhs' rhs'

(==#) :: Int# -> Int# -> Int#
(==#) = compareInt# SymFC.bveq

(/=#) :: Int# -> Int# -> Int#
(/=#) = compareInt# SymFC.bvneq

(>=#) :: Int# -> Int# -> Int#
(>=#) = compareInt# $ flip SymFC.bvsle

(>#) :: Int# -> Int# -> Int#
(>#) = compareInt# $ flip SymFC.bvslt

(<=#) :: Int# -> Int# -> Int#
(<=#) = compareInt# SymFC.bvsle

(<#) :: Int# -> Int# -> Int#
(<#) = compareInt# SymFC.bvslt

int8ToInt# :: Int8# -> Int#
int8ToInt# x = SymFC.toInt# $ SymFC.bvsext $ SymFC.fromInt8# x

int8ToWord8# :: Int8# -> Word8#
int8ToWord8# x = SymFC.toWord8# $ SymFC.fromInt8# x

binaryInt8#
  :: (BitVec8 -> BitVec8 -> BitVec8)
  -> Int8#
  -> Int8#
  -> Int8#
binaryInt8# f lhs rhs = do
  let lhs' = SymFC.fromInt8# lhs
  let rhs' = SymFC.fromInt8# rhs
  SymFC.toInt8# $ f lhs' rhs'

plusInt8# :: Int8# -> Int8# -> Int8#
plusInt8# = binaryInt8# (+)

subInt8# :: Int8# -> Int8# -> Int8#
subInt8# = binaryInt8# (-)

timesInt8# :: Int8# -> Int8# -> Int8#
timesInt8# = binaryInt8# (*)

unaryInt8#
  :: (BitVec8 -> BitVec8)
  -> Int8#
  -> Int8#
unaryInt8# f x = do
  let x' = SymFC.fromInt8# x
  SymFC.toInt8# $ f x'

negateInt8# :: Int8# -> Int8#
negateInt8# = unaryInt8# negate

compareInt8#
  :: (BitVec8 -> BitVec8 -> SymFC.Bool)
  -> Int8#
  -> Int8#
  -> Int#
compareInt8# f lhs rhs = do
  let lhs' = SymFC.fromInt8# lhs
  let rhs' = SymFC.fromInt8# rhs
  bool2I# $ f lhs' rhs'

eqInt8# :: Int8# -> Int8# -> Int#
eqInt8# = compareInt8# SymFC.bveq

neInt8# :: Int8# -> Int8# -> Int#
neInt8# = compareInt8# SymFC.bvneq

geInt8# :: Int8# -> Int8# -> Int#
geInt8# = compareInt8# $ flip SymFC.bvsle

gtInt8# :: Int8# -> Int8# -> Int#
gtInt8# = compareInt8# $ flip SymFC.bvslt

leInt8# :: Int8# -> Int8# -> Int#
leInt8# = compareInt8# SymFC.bvsle

ltInt8# :: Int8# -> Int8# -> Int#
ltInt8# = compareInt8# SymFC.bvslt

int16ToInt# :: Int16# -> Int#
int16ToInt# x = SymFC.toInt# $ SymFC.bvsext $ SymFC.fromInt16# x

int16ToWord16# :: Int16# -> Word16#
int16ToWord16# x = SymFC.toWord16# $ SymFC.fromInt16# x

binaryInt16#
  :: (BitVec16 -> BitVec16 -> BitVec16)
  -> Int16#
  -> Int16#
  -> Int16#
binaryInt16# f lhs rhs = do
  let lhs' = SymFC.fromInt16# lhs
  let rhs' = SymFC.fromInt16# rhs
  SymFC.toInt16# $ f lhs' rhs'

plusInt16# :: Int16# -> Int16# -> Int16#
plusInt16# = binaryInt16# (+)

subInt16# :: Int16# -> Int16# -> Int16#
subInt16# = binaryInt16# (-)

timesInt16# :: Int16# -> Int16# -> Int16#
timesInt16# = binaryInt16# (*)

unaryInt16#
  :: (BitVec16 -> BitVec16)
  -> Int16#
  -> Int16#
unaryInt16# f x = do
  let x' = SymFC.fromInt16# x
  SymFC.toInt16# $ f x'

negateInt16# :: Int16# -> Int16#
negateInt16# = unaryInt16# negate

compareInt16#
  :: (BitVec16 -> BitVec16 -> SymFC.Bool)
  -> Int16#
  -> Int16#
  -> Int#
compareInt16# f lhs rhs = do
  let lhs' = SymFC.fromInt16# lhs
  let rhs' = SymFC.fromInt16# rhs
  bool2I# $ f lhs' rhs'

eqInt16# :: Int16# -> Int16# -> Int#
eqInt16# = compareInt16# SymFC.bveq

neInt16# :: Int16# -> Int16# -> Int#
neInt16# = compareInt16# SymFC.bvneq

geInt16# :: Int16# -> Int16# -> Int#
geInt16# = compareInt16# $ flip SymFC.bvsle

gtInt16# :: Int16# -> Int16# -> Int#
gtInt16# = compareInt16# $ flip SymFC.bvslt

leInt16# :: Int16# -> Int16# -> Int#
leInt16# = compareInt16# SymFC.bvsle

ltInt16# :: Int16# -> Int16# -> Int#
ltInt16# = compareInt16# SymFC.bvslt

int32ToInt# :: Int32# -> Int#
int32ToInt# x = SymFC.toInt# $ SymFC.bvsext $ SymFC.fromInt32# x

int32ToWord32# :: Int32# -> Word32#
int32ToWord32# x = SymFC.toWord32# $ SymFC.fromInt32# x

binaryInt32#
  :: (BitVec32 -> BitVec32 -> BitVec32)
  -> Int32#
  -> Int32#
  -> Int32#
binaryInt32# f lhs rhs = do
  let lhs' = SymFC.fromInt32# lhs
  let rhs' = SymFC.fromInt32# rhs
  SymFC.toInt32# $ f lhs' rhs'

plusInt32# :: Int32# -> Int32# -> Int32#
plusInt32# = binaryInt32# (+)

subInt32# :: Int32# -> Int32# -> Int32#
subInt32# = binaryInt32# (-)

timesInt32# :: Int32# -> Int32# -> Int32#
timesInt32# = binaryInt32# (*)

unaryInt32#
  :: (BitVec32 -> BitVec32)
  -> Int32#
  -> Int32#
unaryInt32# f x = do
  let x' = SymFC.fromInt32# x
  SymFC.toInt32# $ f x'

negateInt32# :: Int32# -> Int32#
negateInt32# = unaryInt32# negate

compareInt32#
  :: (BitVec32 -> BitVec32 -> SymFC.Bool)
  -> Int32#
  -> Int32#
  -> Int#
compareInt32# f lhs rhs = do
  let lhs' = SymFC.fromInt32# lhs
  let rhs' = SymFC.fromInt32# rhs
  bool2I# $ f lhs' rhs'

eqInt32# :: Int32# -> Int32# -> Int#
eqInt32# = compareInt32# SymFC.bveq

neInt32# :: Int32# -> Int32# -> Int#
neInt32# = compareInt32# SymFC.bvneq

geInt32# :: Int32# -> Int32# -> Int#
geInt32# = compareInt32# $ flip SymFC.bvsle

gtInt32# :: Int32# -> Int32# -> Int#
gtInt32# = compareInt32# $ flip SymFC.bvslt

leInt32# :: Int32# -> Int32# -> Int#
leInt32# = compareInt32# SymFC.bvsle

ltInt32# :: Int32# -> Int32# -> Int#
ltInt32# = compareInt32# SymFC.bvslt

int64ToInt# :: Int64# -> Int#
int64ToInt# x = SymFC.toInt# $ SymFC.bvsext $ SymFC.fromInt64# x

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# x = SymFC.toWord64# $ SymFC.fromInt64# x

binaryInt64#
  :: (BitVec 64 -> BitVec 64 -> BitVec 64)
  -> Int64#
  -> Int64#
  -> Int64#
binaryInt64# f lhs rhs = do
  let lhs' = SymFC.fromInt64# lhs
  let rhs' = SymFC.fromInt64# rhs
  SymFC.toInt64# $ f lhs' rhs'

plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# = binaryInt64# (+)

subInt64# :: Int64# -> Int64# -> Int64#
subInt64# = binaryInt64# (-)

timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# = binaryInt64# (*)

unaryInt64#
  :: (BitVec 64 -> BitVec 64)
  -> Int64#
  -> Int64#
unaryInt64# f x = do
  let x' = SymFC.fromInt64# x
  SymFC.toInt64# $ f x'

negateInt64# :: Int64# -> Int64#
negateInt64# = unaryInt64# negate

compareInt64#
  :: (BitVec 64 -> BitVec 64 -> SymFC.Bool)
  -> Int64#
  -> Int64#
  -> Int#
compareInt64# f lhs rhs = do
  let lhs' = SymFC.fromInt64# lhs
  let rhs' = SymFC.fromInt64# rhs
  bool2I# $ f lhs' rhs'

eqInt64# :: Int64# -> Int64# -> Int#
eqInt64# = compareInt64# SymFC.bveq

neInt64# :: Int64# -> Int64# -> Int#
neInt64# = compareInt64# SymFC.bvneq

geInt64# :: Int64# -> Int64# -> Int#
geInt64# = compareInt64# $ flip SymFC.bvsle

gtInt64# :: Int64# -> Int64# -> Int#
gtInt64# = compareInt64# $ flip SymFC.bvslt

leInt64# :: Int64# -> Int64# -> Int#
leInt64# = compareInt64# SymFC.bvsle

ltInt64# :: Int64# -> Int64# -> Int#
ltInt64# = compareInt64# SymFC.bvslt

word2Int# :: Word# -> Int#
word2Int# x = SymFC.toInt# $ SymFC.fromWord# x

wordToWord8# :: Word# -> Word8#
wordToWord8# x = SymFC.toWord8# $ SymFC.bvselect @0 $ SymFC.fromWord# x

wordToWord16# :: Word# -> Word16#
wordToWord16# x = SymFC.toWord16# $ SymFC.bvselect @0 $ SymFC.fromWord# x

wordToWord32# :: Word# -> Word32#
wordToWord32# x = SymFC.toWord32# $ SymFC.bvselect @0 $ SymFC.fromWord# x

wordToWord64# :: Word# -> Word64#
wordToWord64# x = SymFC.toWord64# $ SymFC.bvselect @0 $ SymFC.fromWord# x

binaryWord#
  :: (BitVecPW -> BitVecPW -> BitVecPW)
  -> Word#
  -> Word#
  -> Word#
binaryWord# f lhs rhs = do
  let lhs' = SymFC.fromWord# lhs
  let rhs' = SymFC.fromWord# rhs
  SymFC.toWord# $ f lhs' rhs'

plusWord# :: Word# -> Word# -> Word#
plusWord# = binaryWord# (+)

minusWord# :: Word# -> Word# -> Word#
minusWord# = binaryWord# (-)

timesWord# :: Word# -> Word# -> Word#
timesWord# = binaryWord# (*)

binaryWordC#
  :: (forall n. KnownNat n => BitVec n -> BitVec n -> BitVec n)
  -> Word#
  -> Word#
  -> (# Word#, Int# #)
binaryWordC# f lhs rhs = do
  let project' x
        = SymFC.bvzext @_ @(PlatformWordSize + 1)
        $ SymFC.fromWord# x
  let lhs' = project' lhs
  let rhs' = project' rhs

  let result = f lhs' rhs'
  let add = SymFC.bvselect @0 result
  let carry = SymFC.bvselect @PlatformWordSize @1 result
  let carry' = SymFC.bvzext carry
  (# SymFC.toWord# add, SymFC.toInt# carry' #)

addWordC# :: Word# -> Word# -> (# Word#, Int# #)
addWordC# = binaryWordC# SymFC.bvadd

subWordC# :: Word# -> Word# -> (# Word#, Int# #)
subWordC# = binaryWordC# \lhs rhs -> SymFC.bvadd lhs (SymFC.bvneg rhs)

and# :: Word# -> Word# -> Word#
and# = binaryWord# SymFC.bvand

or# :: Word# -> Word# -> Word#
or# = binaryWord# SymFC.bvor

xor# :: Word# -> Word# -> Word#
xor# = binaryWord# SymFC.bvxor

not# :: Word# -> Word#
not# x = SymFC.toWord# $ SymFC.bvnot $ SymFC.fromWord# x

-- FIXME: This behaviour is not great: in Haskell, it is UB when the idx is
-- out of bounds but we now give it a specific semantic. Ideally, we would have
-- something like "forall UB that a shift can produce". This would probably
-- amount to an uninterpreted function for the shift. I don't have a way of
-- conjuring this at the moment, so I'll leave it as is for now!
uncheckedShiftL# :: Word# -> Int# -> Word#
uncheckedShiftL# val idx = do
  let val' = SymFC.fromWord# val
  let idx' = SymFC.fromInt# idx
  SymFC.toWord# $ SymFC.bvshl val' idx'

uncheckedShiftRL# :: Word# -> Int# -> Word#
uncheckedShiftRL# val idx = do
  let val' = SymFC.fromWord# val
  let idx' = SymFC.fromInt# idx
  SymFC.toWord# $ SymFC.bvlshr val' idx'

compareWord#
  :: (BitVecPW -> BitVecPW -> SymFC.Bool)
  -> Word#
  -> Word#
  -> Int#
compareWord# f lhs rhs = do
  let lhs' = SymFC.fromWord# lhs
  let rhs' = SymFC.fromWord# rhs
  bool2I# $ f lhs' rhs'

eqWord# :: Word# -> Word# -> Int#
eqWord# = compareWord# SymFC.bveq

neWord# :: Word# -> Word# -> Int#
neWord# = compareWord# SymFC.bvneq

geWord# :: Word# -> Word# -> Int#
geWord# = compareWord# $ flip SymFC.bvule

gtWord# :: Word# -> Word# -> Int#
gtWord# = compareWord# $ flip SymFC.bvult

leWord# :: Word# -> Word# -> Int#
leWord# = compareWord# SymFC.bvule

ltWord# :: Word# -> Word# -> Int#
ltWord# = compareWord# SymFC.bvult

word8ToWord# :: Word8# -> Word#
word8ToWord# x = SymFC.toWord# $ SymFC.bvzext $ SymFC.fromWord8# x

word8ToInt8# :: Word8# -> Int8#
word8ToInt8# x = SymFC.toInt8# $ SymFC.fromWord8# x

binaryWord8#
  :: (BitVec8 -> BitVec8 -> BitVec8)
  -> Word8#
  -> Word8#
  -> Word8#
binaryWord8# f lhs rhs = do
  let lhs' = SymFC.fromWord8# lhs
  let rhs' = SymFC.fromWord8# rhs
  SymFC.toWord8# $ f lhs' rhs'

plusWord8# :: Word8# -> Word8# -> Word8#
plusWord8# = binaryWord8# (+)

subWord8# :: Word8# -> Word8# -> Word8#
subWord8# = binaryWord8# (-)

timesWord8# :: Word8# -> Word8# -> Word8#
timesWord8# = binaryWord8# (*)

andWord8# :: Word8# -> Word8# -> Word8#
andWord8# = binaryWord8# SymFC.bvand

orWord8# :: Word8# -> Word8# -> Word8#
orWord8# = binaryWord8# SymFC.bvor

xorWord8# :: Word8# -> Word8# -> Word8#
xorWord8# = binaryWord8# SymFC.bvxor

notWord8# :: Word8# -> Word8#
notWord8# x = SymFC.toWord8# $ SymFC.bvnot $ SymFC.fromWord8# x

compareWord8#
  :: (BitVec8 -> BitVec8 -> SymFC.Bool)
  -> Word8#
  -> Word8#
  -> Int#
compareWord8# f lhs rhs = do
  let lhs' = SymFC.fromWord8# lhs
  let rhs' = SymFC.fromWord8# rhs
  bool2I# $ f lhs' rhs'

eqWord8# :: Word8# -> Word8# -> Int#
eqWord8# = compareWord8# SymFC.bveq

neWord8# :: Word8# -> Word8# -> Int#
neWord8# = compareWord8# SymFC.bvneq

geWord8# :: Word8# -> Word8# -> Int#
geWord8# = compareWord8# $ flip SymFC.bvule

gtWord8# :: Word8# -> Word8# -> Int#
gtWord8# = compareWord8# $ flip SymFC.bvult

leWord8# :: Word8# -> Word8# -> Int#
leWord8# = compareWord8# SymFC.bvule

ltWord8# :: Word8# -> Word8# -> Int#
ltWord8# = compareWord8# SymFC.bvult

word16ToWord# :: Word16# -> Word#
word16ToWord# x = SymFC.toWord# $ SymFC.bvzext $ SymFC.fromWord16# x

word16ToInt16# :: Word16# -> Int16#
word16ToInt16# x = SymFC.toInt16# $ SymFC.fromWord16# x

binaryWord16#
  :: (BitVec16 -> BitVec16 -> BitVec16)
  -> Word16#
  -> Word16#
  -> Word16#
binaryWord16# f lhs rhs = do
  let lhs' = SymFC.fromWord16# lhs
  let rhs' = SymFC.fromWord16# rhs
  SymFC.toWord16# $ f lhs' rhs'

plusWord16# :: Word16# -> Word16# -> Word16#
plusWord16# = binaryWord16# (+)

subWord16# :: Word16# -> Word16# -> Word16#
subWord16# = binaryWord16# (-)

timesWord16# :: Word16# -> Word16# -> Word16#
timesWord16# = binaryWord16# (*)

andWord16# :: Word16# -> Word16# -> Word16#
andWord16# = binaryWord16# SymFC.bvand

orWord16# :: Word16# -> Word16# -> Word16#
orWord16# = binaryWord16# SymFC.bvor

xorWord16# :: Word16# -> Word16# -> Word16#
xorWord16# = binaryWord16# SymFC.bvxor

notWord16# :: Word16# -> Word16#
notWord16# x = SymFC.toWord16# $ SymFC.bvnot $ SymFC.fromWord16# x

compareWord16#
  :: (BitVec16 -> BitVec16 -> SymFC.Bool)
  -> Word16#
  -> Word16#
  -> Int#
compareWord16# f lhs rhs = do
  let lhs' = SymFC.fromWord16# lhs
  let rhs' = SymFC.fromWord16# rhs
  bool2I# $ f lhs' rhs'

eqWord16# :: Word16# -> Word16# -> Int#
eqWord16# = compareWord16# SymFC.bveq

neWord16# :: Word16# -> Word16# -> Int#
neWord16# = compareWord16# SymFC.bvneq

geWord16# :: Word16# -> Word16# -> Int#
geWord16# = compareWord16# $ flip SymFC.bvule

gtWord16# :: Word16# -> Word16# -> Int#
gtWord16# = compareWord16# $ flip SymFC.bvult

leWord16# :: Word16# -> Word16# -> Int#
leWord16# = compareWord16# SymFC.bvule

ltWord16# :: Word16# -> Word16# -> Int#
ltWord16# = compareWord16# SymFC.bvult

word32ToWord# :: Word32# -> Word#
word32ToWord# x = SymFC.toWord# $ SymFC.bvzext $ SymFC.fromWord32# x

word32ToInt32# :: Word32# -> Int32#
word32ToInt32# x = SymFC.toInt32# $ SymFC.fromWord32# x

binaryWord32#
  :: (BitVec32 -> BitVec32 -> BitVec32)
  -> Word32#
  -> Word32#
  -> Word32#
binaryWord32# f lhs rhs = do
  let lhs' = SymFC.fromWord32# lhs
  let rhs' = SymFC.fromWord32# rhs
  SymFC.toWord32# $ f lhs' rhs'

plusWord32# :: Word32# -> Word32# -> Word32#
plusWord32# = binaryWord32# (+)

subWord32# :: Word32# -> Word32# -> Word32#
subWord32# = binaryWord32# (-)

timesWord32# :: Word32# -> Word32# -> Word32#
timesWord32# = binaryWord32# (*)

andWord32# :: Word32# -> Word32# -> Word32#
andWord32# = binaryWord32# SymFC.bvand

orWord32# :: Word32# -> Word32# -> Word32#
orWord32# = binaryWord32# SymFC.bvor

xorWord32# :: Word32# -> Word32# -> Word32#
xorWord32# = binaryWord32# SymFC.bvxor

notWord32# :: Word32# -> Word32#
notWord32# x = SymFC.toWord32# $ SymFC.bvnot $ SymFC.fromWord32# x

compareWord32#
  :: (BitVec32 -> BitVec32 -> SymFC.Bool)
  -> Word32#
  -> Word32#
  -> Int#
compareWord32# f lhs rhs = do
  let lhs' = SymFC.fromWord32# lhs
  let rhs' = SymFC.fromWord32# rhs
  bool2I# $ f lhs' rhs'

eqWord32# :: Word32# -> Word32# -> Int#
eqWord32# = compareWord32# SymFC.bveq

neWord32# :: Word32# -> Word32# -> Int#
neWord32# = compareWord32# SymFC.bvneq

geWord32# :: Word32# -> Word32# -> Int#
geWord32# = compareWord32# $ flip SymFC.bvule

gtWord32# :: Word32# -> Word32# -> Int#
gtWord32# = compareWord32# $ flip SymFC.bvult

leWord32# :: Word32# -> Word32# -> Int#
leWord32# = compareWord32# SymFC.bvule

ltWord32# :: Word32# -> Word32# -> Int#
ltWord32# = compareWord32# SymFC.bvult

word64ToWord# :: Word64# -> Word#
word64ToWord# x = SymFC.toWord# $ SymFC.bvzext $ SymFC.fromWord64# x

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# x = SymFC.toInt64# $ SymFC.fromWord64# x

binaryWord64#
  :: (BitVec 64 -> BitVec 64 -> BitVec 64)
  -> Word64#
  -> Word64#
  -> Word64#
binaryWord64# f lhs rhs = do
  let lhs' = SymFC.fromWord64# lhs
  let rhs' = SymFC.fromWord64# rhs
  SymFC.toWord64# $ f lhs' rhs'

plusWord64# :: Word64# -> Word64# -> Word64#
plusWord64# = binaryWord64# (+)

subWord64# :: Word64# -> Word64# -> Word64#
subWord64# = binaryWord64# (-)

timesWord64# :: Word64# -> Word64# -> Word64#
timesWord64# = binaryWord64# (*)

and64# :: Word64# -> Word64# -> Word64#
and64# = binaryWord64# SymFC.bvand

or64# :: Word64# -> Word64# -> Word64#
or64# = binaryWord64# SymFC.bvor

xor64# :: Word64# -> Word64# -> Word64#
xor64# = binaryWord64# SymFC.bvxor

not64# :: Word64# -> Word64#
not64# x = SymFC.toWord64# $ SymFC.bvnot $ SymFC.fromWord64# x

compareWord64#
  :: (BitVec 64 -> BitVec 64 -> SymFC.Bool)
  -> Word64#
  -> Word64#
  -> Int#
compareWord64# f lhs rhs = do
  let lhs' = SymFC.fromWord64# lhs
  let rhs' = SymFC.fromWord64# rhs
  bool2I# $ f lhs' rhs'

eqWord64# :: Word64# -> Word64# -> Int#
eqWord64# = compareWord64# SymFC.bveq

neWord64# :: Word64# -> Word64# -> Int#
neWord64# = compareWord64# SymFC.bvneq

geWord64# :: Word64# -> Word64# -> Int#
geWord64# = compareWord64# $ flip SymFC.bvule

gtWord64# :: Word64# -> Word64# -> Int#
gtWord64# = compareWord64# $ flip SymFC.bvult

leWord64# :: Word64# -> Word64# -> Int#
leWord64# = compareWord64# SymFC.bvule

ltWord64# :: Word64# -> Word64# -> Int#
ltWord64# = compareWord64# SymFC.bvult

fromInteger :: Integer -> SymFC.Integer
fromInteger = \case
  IS x -> SymFC.bvs2i @PlatformWordSize $ SymFC.fromInt# x
  IP _x -> undefined
  IN _x -> undefined

toInteger :: SymFC.Integer -> Integer
toInteger x = do
  let toI (I# i) = SymFC.bvs2i $ SymFC.fromInt# i
  let minI = toI minBound
  let maxI = toI maxBound
  if
    -- TODO: We should actually implement the 'undefined'. It depends on the
    -- interpretation of ByteArray# though, which we currently do not use. Same
    -- for the inverse function btw.
    | x < minI -> undefined
    | maxI < x -> undefined
    | otherwise -> do
      let x' = SymFC.i2bv @PlatformWordSize x
      IS $ SymFC.toInt# x'

-- TODO: The below definitions exists solely because the unfolding doesn't
-- exist. There should be a way around this...
--
-- The main problem is that GHC comes with its own version of 'base' and it
-- thus cannot be compiled with -fexpose-all-unfoldings.

integerFromNatural :: Natural -> Integer
integerFromNatural (NS x) = GHC.integerFromWord# x
integerFromNatural (NB x) = IP x

integerToNatural :: Integer -> Natural
integerToNatural = \case
  IS x -> GHC.naturalFromWord# (GHC.wordFromAbsInt# x)
  IP x -> GHC.naturalFromBigNat# x
  IN x -> GHC.naturalFromBigNat# x

integerToNaturalThrow :: Integer -> Natural
integerToNaturalThrow (IS x)
  | GHC.isTrue# (x <# 0#) = GHC.raiseUnderflow
  | True                  = GHC.naturalFromWord# (int2Word# x)
integerToNaturalThrow (IP x) = GHC.naturalFromBigNat# x
integerToNaturalThrow (IN _) = GHC.raiseUnderflow

integerFromWord# :: Word# -> Integer
integerFromWord# w = if
  | let i = GHC.word2Int# w
  , GHC.isTrue# (i GHC.>=# 0#) -> IS i
  | otherwise -> IP (GHC.bigNatFromWord# w)

integerToInt# :: Integer -> Int#
integerToInt# = \case
  IS i -> i
  IP b -> GHC.word2Int# $ GHC.bigNatToWord# b
  IN b -> GHC.negateInt# $ GHC.word2Int# $ GHC.bigNatToWord# b

integerToWord# :: Integer -> Word#
integerToWord# = \case
  IS i -> GHC.int2Word# i
  IP bn -> GHC.bigNatToWord# bn
  IN bn -> GHC.int2Word# $ GHC.negateInt# $ GHC.word2Int# $ GHC.bigNatToWord# bn

integerSub :: Integer -> Integer -> Integer
integerSub !x      (IS 0#) = x     -- Note [Bangs in Integer functions]
integerSub (IS x#) (IS y#)
  = case GHC.subIntC# x# y# of
    (# z#, 0# #) -> IS z#
    (# 0#, _  #) -> IN (GHC.bigNatFromWord2# 1## 0##)
    (# z#, _  #)
      | GHC.isTrue# (z# GHC.># 0#)
      -> IN (GHC.bigNatFromWord# (GHC.int2Word# (GHC.negateInt# z#)))
      | True
      -> IP (GHC.bigNatFromWord# (GHC.int2Word# z#))
integerSub (IS x#) (IP y)
  | GHC.isTrue# (x# GHC.>=# 0#)
  = GHC.integerFromBigNatNeg# (GHC.bigNatSubWordUnsafe# y (GHC.int2Word# x#))
  | otherwise
  = IN (GHC.bigNatAddWord# y (GHC.int2Word# (GHC.negateInt# x#)))
integerSub (IS x#) (IN y)
  | GHC.isTrue# (x# GHC.>=# 0#)
  = IP (GHC.bigNatAddWord# y (GHC.int2Word# x#))
  | otherwise
  = GHC.integerFromBigNat# (GHC.bigNatSubWordUnsafe# y (GHC.int2Word# (GHC.negateInt# x#)))
integerSub (IP x) (IP y)
  = case GHC.bigNatCompare x y of
    LT -> GHC.integerFromBigNatNeg# (GHC.bigNatSubUnsafe y x)
    EQ -> IS 0#
    GT -> GHC.integerFromBigNat# (GHC.bigNatSubUnsafe x y)
integerSub (IP x) (IN y) = IP (GHC.bigNatAdd x y)
integerSub (IN x) (IP y) = IN (GHC.bigNatAdd x y)
integerSub (IN x) (IN y)
  = case GHC.bigNatCompare x y of
    LT -> GHC.integerFromBigNat# (GHC.bigNatSubUnsafe y x)
    EQ -> IS 0#
    GT -> GHC.integerFromBigNatNeg# (GHC.bigNatSubUnsafe x y)
integerSub (IP x) (IS y#)
  | GHC.isTrue# (y# GHC.>=# 0#)
  = GHC.integerFromBigNat# (GHC.bigNatSubWordUnsafe# x (GHC.int2Word# y#))
  | otherwise
  = IP (GHC.bigNatAddWord# x (GHC.int2Word# (GHC.negateInt# y#)))
integerSub (IN x) (IS y#)
  | GHC.isTrue# (y# GHC.>=# 0#)
  = IN (GHC.bigNatAddWord# x (GHC.int2Word# y#))
  | otherwise
  = GHC.integerFromBigNatNeg# (GHC.bigNatSubWordUnsafe# x (GHC.int2Word# (GHC.negateInt# y#)))

naturalAdd :: Natural -> Natural -> Natural
naturalAdd = \cases
  (NS x) (NB y) -> NB $ GHC.bigNatAddWord# y x
  (NB x) (NS y) -> NB $ GHC.bigNatAddWord# x y
  (NB x) (NB y) -> NB $ GHC.bigNatAdd x y
  (NS x) (NS y) -> case GHC.addWordC# x y of
    (# l, 0# #) -> NS l
    (# l, c  #) -> NB $ GHC.bigNatFromWord2# (GHC.int2Word# c) l

naturalSubThrow :: Natural -> Natural -> Natural
naturalSubThrow (NS _) (NB _) = GHC.raiseUnderflow
naturalSubThrow (NB x) (NS y) = GHC.naturalFromBigNat# $ GHC.bigNatSubWordUnsafe# x y
naturalSubThrow (NS x) (NS y) = case GHC.subWordC# x y of
  (# l, 0# #) -> NS l
  (# _, _  #) -> GHC.raiseUnderflow
naturalSubThrow (NB x) (NB y) = case GHC.bigNatSub x y of
  (# (# #) |   #) -> GHC.raiseUnderflow
  (#       | z #) -> GHC.naturalFromBigNat# z

noinline :: a -> a
noinline = id

-- FIXME: This is not actually the implementation for 'undefined'.
undefined :: a
undefined = GHC.raise# ()

-- FIXME: This is not actually the implementation for 'throw'.
throw :: forall rep (a :: TYPE rep) e. e -> a
throw = GHC.raise# ()

-- FIXME: This is not actually the implementation for 'patError'.
patError :: forall q (a :: TYPE q). Addr# -> a
patError _ = GHC.raise# ()

-- FIXME: This is not actually the implementation for 'absentError'.
absentError :: Addr# -> a
absentError _ = GHC.raise# ()

withSomeSNat
  :: forall rep (r :: TYPE rep)
   . Natural
  -> (forall n. SNat n -> r)
  -> r
withSomeSNat n f = f $ unsafeSNat n

map :: (a -> b) -> [a] -> [b]
map f = do
  let go = \case
        [] -> []
        x : xs -> f x : go xs
  go

zip :: [a] -> [b] -> [(a, b)]
zip = \cases
  (x : xs) (y : ys) -> (x, y) : zip xs ys
  _ _ -> []

runRW# :: forall r (o :: TYPE r). (State# RealWorld -> o) -> o 
runRW# m = m GHC.realWorld#

instance Symbolic Int where
  symbolic = do
    bv <- symbolic
    pure $ I# (SymFC.toInt# bv)

instance Symbolic Int8 where
  symbolic = do
    bv <- symbolic
    pure $ I8# (SymFC.toInt8# bv)

instance Symbolic Int16 where
  symbolic = do
    bv <- symbolic
    pure $ I16# (SymFC.toInt16# bv)

instance Symbolic Int32 where
  symbolic = do
    bv <- symbolic
    pure $ I32# (SymFC.toInt32# bv)

instance Symbolic Int64 where
  symbolic = do
    bv <- symbolic
    pure $ I64# (SymFC.toInt64# bv)

instance Symbolic Word where
  symbolic = do
    bv <- symbolic
    pure $ W# (SymFC.toWord# bv)

instance Symbolic Word8 where
  symbolic = do
    bv <- symbolic
    pure $ W8# (SymFC.toWord8# bv)

instance Symbolic Word16 where
  symbolic = do
    bv <- symbolic
    pure $ W16# (SymFC.toWord16# bv)

instance Symbolic Word32 where
  symbolic = do
    bv <- symbolic
    pure $ W32# (SymFC.toWord32# bv)

instance Symbolic Word64 where
  symbolic = do
    bv <- symbolic
    pure $ W64# (SymFC.toWord64# bv)

instance Symbolic Integer where
  symbolic = do
    guard <- symbolic
    bv <- symbolic
    pure $ SymFC.ite guard (IS $ SymFC.toInt# bv) undefined

instance Symbolic ()

instance Symbolic Void

instance Symbolic Bool

instance Symbolic a => Symbolic (Identity a)

instance Symbolic a => Symbolic [a]

instance Symbolic a => Symbolic (Maybe a)

instance (Symbolic a, Symbolic b) => Symbolic (Either a b)

instance (Symbolic a, Symbolic b) => Symbolic (a, b)

instance (Symbolic a, Symbolic b, Symbolic c) => Symbolic (a, b, c)

instance (Symbolic a, Symbolic b, Symbolic c, Symbolic d) => Symbolic (a, b, c, d)
