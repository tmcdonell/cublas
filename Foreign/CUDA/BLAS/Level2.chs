{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.BLAS.Level2 (

  -- * Types
  Operation(..),

  -- * Single-precision float
  sgemv,

) where

-- Friends
import Foreign.CUDA.BLAS.Error
import Foreign.CUDA.BLAS.Context
import Foreign.CUDA.BLAS.Internal.C2HS

import Foreign.CUDA.Ptr

-- System
import Foreign
import Foreign.C

#include <cublas_v2.h>
{# context lib="cublas" #}


-- | The Operation type indicates which operation needs to be performed with the
-- dense matrix. Its values correspond to Fortran characters ‘N’ or ‘n’
-- (non-transpose), ‘T’ or ‘t’ (transpose) and ‘C’ or ‘c’ (conjugate transpose)
-- that are often used as parameters to legacy BLAS implementations.
--
{# enum cublasOperation_t as Operation
  { underscoreToCase }
  with prefix="CUBLAS_OP" deriving (Eq, Show) #}


-- Level-2 BLAS operations -----------------------------------------------------
--

-- | This function performs the matrix-vector multiplication:
--
--   y = alpha * op(A)x + beta * y
--
-- where A is a m × n matrix stored in column-major format, x and y are vectors,
-- and alpha and beta are scalars.
--
sgemv :: Handle
      -> Operation
      -> Int                    -- ^ m: rows of A
      -> Int                    -- ^ n: columns of A
      -> Float                  -- ^ alpha
      -> DevicePtr Float        -- ^ A
      -> Int
      -> DevicePtr Float        -- ^ x
      -> Int
      -> Float                  -- ^ beta
      -> DevicePtr Float        -- ^ y
      -> Int
      -> IO ()
sgemv hdl op m n alpha a lda x incx beta y incy
  = nothingIfOk =<< cublasSgemv hdl op m n alpha a lda x incx beta y incy

{# fun unsafe cublasSgemv_v2 as cublasSgemv
  { useHandle   `Handle'
  , cFromEnum   `Operation'
  ,             `Int'
  ,             `Int'
  , with'*      `Float'
  , useDev      `DevicePtr Float'
  ,             `Int'
  , useDev      `DevicePtr Float'
  ,             `Int'
  , with'*      `Float'
  , useDev      `DevicePtr Float'
  ,             `Int'                   } -> `Status' cToEnum #}
  where
    with' x f   = with x (f . castPtr)
    useDev      = castPtr . useDevicePtr

