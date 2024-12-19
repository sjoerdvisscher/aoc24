-- GENERATED by C->Haskell Compiler, version 0.28.8 Switcheroo, 25 November 2017 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
module Numeric.Limp.Solvers.Cbc.Internal.Foreign where
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Marshal.Utils as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified System.IO.Unsafe as C2HSImp



import Foreign
import Foreign.C

import Control.Applicative

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)

import Unsafe.Coerce




newtype CbcModel = CbcModel (ForeignPtr CbcModel)
type CbcModelPtr = C2HSImp.Ptr (CbcModel)
{-# LINE 18 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



foreign import ccall "&freeModel"
   cbcDeleteModel_funptr :: FunPtr (Ptr CbcModel -> IO ())

mkCbcModel :: CbcModelPtr -> IO CbcModel
mkCbcModel p
    = CbcModel <$> newForeignPtr cbcDeleteModel_funptr p

withCbcModel :: CbcModel -> (CbcModelPtr -> IO b) -> IO b
withCbcModel (CbcModel ptr) f
    = do    withForeignPtr ptr f

newModel :: IO ((CbcModel))
newModel =
  newModel'_ >>= \res ->
  mkCbcModel res >>= \res' ->
  return (res')

{-# LINE 33 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}


withVecI    :: Vector Int -> (Ptr CInt -> IO b) -> IO b
withVecI v f
 = V.unsafeWith (V.map fromIntegral v) f

withVecD    :: Vector Double -> (Ptr CDouble -> IO b) -> IO b
withVecD v f
 = V.unsafeWith v (f . castPtr)


loadProblem :: (CbcModel) -> (Int) -- # rows
 -> (Int) -- # columns
 -> (Vector Int) -- starts
 -> (Vector Int) -- indices
 -> (Vector Double) -- values
 -> (Vector Double) -- col lower bounds
 -> (Vector Double) -- col upper bounds
 -> (Vector Double) -- objective coefficients
 -> (Vector Double) -- row lower bounds
 -> (Vector Double) -- row upper bounds
 -> IO ()
loadProblem a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  withCbcModel a1 $ \a1' -> 
  let {a2' = fromIntegral a2} in 
  let {a3' = fromIntegral a3} in 
  withVecI a4 $ \a4' -> 
  withVecI a5 $ \a5' -> 
  withVecD a6 $ \a6' -> 
  withVecD a7 $ \a7' -> 
  withVecD a8 $ \a8' -> 
  withVecD a9 $ \a9' -> 
  withVecD a10 $ \a10' -> 
  withVecD a11 $ \a11' -> 
  loadProblem'_ a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11' >>
  return ()

{-# LINE 61 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



setInteger :: (CbcModel) -> (Int) -> IO ()
setInteger a1 a2 =
  withCbcModel a1 $ \a1' -> 
  let {a2' = fromIntegral a2} in 
  setInteger'_ a1' a2' >>
  return ()

{-# LINE 67 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



branchAndBound :: (CbcModel) -> IO ()
branchAndBound a1 =
  withCbcModel a1 $ \a1' -> 
  branchAndBound'_ a1' >>
  return ()

{-# LINE 72 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



getSolution :: CbcModel -> IO (Vector Double)
getSolution m@(CbcModel fp)
 = do   ncols <- fromIntegral <$> withCbcModel m getNumCols
{-# LINE 77 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}

        vd    <- unsafeCoerce <$> withCbcModel m getBestSolution
{-# LINE 78 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}

        arr <- V.generateM ncols (peekElemOff vd)

        -- The model owns the array, so ensure we keep it around until after we've read the whole array
        touchForeignPtr fp
        return arr


setObjSense :: (CbcModel) -> (Double) -> IO ()
setObjSense a1 a2 =
  withCbcModel a1 $ \a1' -> 
  let {a2' = realToFrac a2} in 
  setObjSense'_ a1' a2' >>
  return ()

{-# LINE 89 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



setLogLevel :: (CbcModel) -> (Int) -> IO ()
setLogLevel a1 a2 =
  withCbcModel a1 $ \a1' -> 
  let {a2' = fromIntegral a2} in 
  setLogLevel'_ a1' a2' >>
  return ()

{-# LINE 95 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



isProvenInfeasible :: (CbcModel) -> IO ((Bool))
isProvenInfeasible a1 =
  withCbcModel a1 $ \a1' -> 
  isProvenInfeasible'_ a1' >>= \res ->
  let {res' = C2HSImp.toBool res} in
  return (res')

{-# LINE 100 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



getCoinDblMax :: (Double)
getCoinDblMax =
  C2HSImp.unsafePerformIO $
  getCoinDblMax'_ >>= \res ->
  let {res' = realToFrac res} in
  return (res')

{-# LINE 104 "src/Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs" #-}



foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h newModel"
  newModel'_ :: (IO (CbcModelPtr))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h loadProblem"
  loadProblem'_ :: ((CbcModelPtr) -> (C2HSImp.CInt -> (C2HSImp.CInt -> ((C2HSImp.Ptr C2HSImp.CInt) -> ((C2HSImp.Ptr C2HSImp.CInt) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> ((C2HSImp.Ptr C2HSImp.CDouble) -> (IO ()))))))))))))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h setInteger"
  setInteger'_ :: ((CbcModelPtr) -> (C2HSImp.CInt -> (IO ())))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h branchAndBound"
  branchAndBound'_ :: ((CbcModelPtr) -> (IO ()))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h getNumCols"
  getNumCols :: ((CbcModelPtr) -> (IO C2HSImp.CInt))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h getBestSolution"
  getBestSolution :: ((CbcModelPtr) -> (IO (C2HSImp.Ptr C2HSImp.CDouble)))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h setObjSense"
  setObjSense'_ :: ((CbcModelPtr) -> (C2HSImp.CDouble -> (IO ())))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h setLogLevel"
  setLogLevel'_ :: ((CbcModelPtr) -> (C2HSImp.CInt -> (IO ())))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h isProvenInfeasible"
  isProvenInfeasible'_ :: ((CbcModelPtr) -> (IO C2HSImp.CInt))

foreign import ccall safe "Numeric/Limp/Solvers/Cbc/Internal/Foreign.chs.h getCoinDblMax"
  getCoinDblMax'_ :: (IO C2HSImp.CDouble)