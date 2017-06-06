-- |
-- Module      : Foreign.CUDA.BLAS.Level1
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuBLAS Level-1 function reference:
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-1-function-reference>
--

module Foreign.CUDA.BLAS.Level1 (

  -- * Level 1 operations

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublasi-lt-t-gt-amax>
  isamax, idamax, icamax, izamax,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublasi-lt-t-gt-amin>
  isamin, idamin, icamin, izamin,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-asum>
  sasum, dasum, scasum, dzasum,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-axpy>
  saxpy, daxpy, caxpy, zaxpy,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-copy>
  scopy, dcopy, ccopy, zcopy,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-dot>
  sdot, ddot, cdotu, zdotu, cdotc, zdotc,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-nrm2>
  snrm2, dnrm2, scnrm2, dznrm2,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-rot>
  srot, drot, crot, csrot, zrot, zdrot,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-rotg>
  srotg, drotg, crotg, zrotg,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-rotm>
  srotm, drotm,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-rotmg>
  srotmg, drotmg,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-scal>
  sscal, dscal, cscal, csscal, zscal, zdscal,

  -- | <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-swap>
  sswap, dswap, cswap, zswap,

) where

import Foreign.CUDA.BLAS.Internal.FFI

