{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Data.Vector as V
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as HMat


newtype Vec = Vec { unV :: HMat.Vector Double }
  deriving (Eq, Ord, Show)

-- Does crash with GHC 8.6.5

dims :: Int
dims = 3

data instance VU.MVector s Vec = MVector_Vec (VU.MVector s Double)
data instance VU.Vector    Vec = Vector_Vec  (VU.Vector    Double)

instance VGM.MVector VU.MVector Vec where
  basicLength (MVector_Vec v) = VGM.basicLength v `div` dims
  basicUnsafeSlice start size (MVector_Vec v) = MVector_Vec (VGM.basicUnsafeSlice (start * dims) (size * dims) v)
  basicOverlaps (MVector_Vec v1) (MVector_Vec v2) = VGM.basicOverlaps v1 v2
  basicUnsafeNew size = MVector_Vec <$> VGM.basicUnsafeNew (size * dims)
  basicInitialize (MVector_Vec vec) = VGM.basicInitialize vec
  basicUnsafeRead vec idx = fmap (Vec . VG.convert) $ VU.freeze sliceVals
    where MVector_Vec sliceVals = VGM.unsafeSlice idx 1 vec
  basicUnsafeWrite vec idx (Vec val) = VG.unsafeCopy sliceVals (VG.convert val)
    where MVector_Vec sliceVals = VGM.unsafeSlice idx 1 vec
  basicUnsafeCopy (MVector_Vec target) (MVector_Vec source) = VGM.basicUnsafeCopy target source
  basicUnsafeMove (MVector_Vec target) (MVector_Vec source) = VGM.basicUnsafeMove target source

instance VG.Vector VU.Vector Vec where
  basicUnsafeFreeze (MVector_Vec mvec) = Vector_Vec <$> VG.basicUnsafeFreeze mvec
  basicUnsafeThaw (Vector_Vec vec) = MVector_Vec <$> VG.basicUnsafeThaw vec
  basicLength (Vector_Vec vec) = VG.basicLength vec `div` dims
  basicUnsafeSlice idx size (Vector_Vec vec) = Vector_Vec $ VG.basicUnsafeSlice (idx * dims) (size * dims) vec
  basicUnsafeIndexM (Vector_Vec vec) idx = return $! Vec $ VS.convert $! VG.unsafeSlice (idx * dims) dims vec
  basicUnsafeCopy (MVector_Vec mvec) (Vector_Vec vec) = VG.unsafeCopy mvec vec
  elemseq _ x y = x `seq` y

instance VU.Unbox Vec

-- Does not crash:

-- $(derivingUnbox "Vec"
--   [t| Vec -> (Double, Double, Double) |]
--   [| \(Vec hm) -> let [a, b, c] = HMat.toList hm in (a, b, c) |]
--   [| \(a, b, c) -> Vec (HMat.fromList [a, b, c]) |]
--  )

newtype NormalVec = NormalVec { getNormalVec :: Vec }
  deriving (Eq, Ord, Show)

$(derivingUnbox "NormalVec"
  [t| NormalVec -> Vec |]
  [| getNormalVec |]
  [| NormalVec |]
 )

unboxedVectorVecRoundtrip :: V.Vector Vec -> Bool
unboxedVectorVecRoundtrip v =
  let
    !indexVector = VU.fromList [0,1,2]
    !v' = VG.map (v VG.!) indexVector
  in
    VG.convert v' == V.map (v VG.!) (VG.convert indexVector)

unboxedVectorNormalVecRoundtrip :: V.Vector NormalVec -> Bool
unboxedVectorNormalVecRoundtrip v =
  let
    !indexVector = VU.fromList [0,1,2]
    !v' = VG.map (v VG.!) indexVector
  in
    VG.convert v' == V.map (v VG.!) (VG.convert indexVector)


main :: IO ()
main = do
  putStrLn "Unboxed.Vector (Vec World) roundtrip (does not crash in repro)"

  let v = Vec (HMat.fromList [1,2,3])

  print $ unboxedVectorVecRoundtrip $
    V.fromList [ v, v, v ]


  putStrLn "Unboxed.Vector (NormalVec World) roundtrip (CRASHES in repro)"

  print $ unboxedVectorNormalVecRoundtrip $
    V.fromList [ NormalVec v, NormalVec v, NormalVec v ]
