{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Context
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Context (

  -- * Context management
  Handle(..),
  create, destroy,

  -- ** Utilities
  PointerMode(..), AtomicsMode(..), MathMode(..),
  setPointerMode,
  getPointerMode,
  setAtomicsMode,
  getAtomicsMode,
  setMathMode,
  getMathMode,

) where

-- Friends
import Foreign.CUDA.BLAS.Error
import Foreign.CUDA.BLAS.Internal.C2HS
import Foreign.CUDA.BLAS.Internal.Types

-- System
import Foreign
import Foreign.C
import Control.Monad                            ( liftM )

#include "cbits/stubs.h"
{# context lib="cublas" #}


-- | This function initializes the CUBLAS library and creates a handle to an
-- opaque structure holding the CUBLAS library context. It allocates hardware
-- resources on the host and device and must be called prior to making any other
-- CUBLAS library calls.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublascreate>
--
{-# INLINEABLE create #-}
{# fun unsafe cublasCreate_v2 as create
  { alloca- `Handle' peekHdl* } -> `()' checkStatus*- #}
  where
    peekHdl = liftM Handle . peek


-- | This function releases hardware resources used by the CUBLAS library. The
-- release of GPU resources may be deferred until the application exits. This
-- function is usually the last call with a particular handle to the CUBLAS
-- library.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublasdestroy>
--
{-# INLINEABLE destroy #-}
{# fun unsafe cublasDestroy_v2 as destroy
  { useHandle `Handle' } -> `()' checkStatus* #}


-- | Set the pointer mode used by cuBLAS library functions. For example, this
-- controls whether the scaling parameters \(\alpha\) and \(\beta\) of the
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-gemm ?gemm>
-- operation are treated as residing in host or device memory.
--
-- The default mode is for values to be passed by reference from the host.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublassetpointermode>
--
{-# INLINEABLE setPointerMode #-}
{# fun unsafe cublasSetPointerMode_v2 as setPointerMode
  { useHandle `Handle'
  , cFromEnum `PointerMode'
  }
  -> `()' checkStatus* #}

-- | Get the pointer mode used by cuBLAS library functions to pass scalar
-- arguments.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublasgetpointermode>
--
{-# INLINEABLE getPointerMode #-}
{# fun unsafe cublasGetPointerMode_v2 as getPointerMode
  { useHandle `Handle'
  , alloca-   `PointerMode' peekEnum*
  }
  -> `()' checkStatus*- #}


-- | Set whether cuBLAS library functions are allowed to use atomic functions,
-- when available. The implementations are generally faster, but can generate
-- results which are not strictly identical from one run to another.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublassetatomicsmode>
--
{-# INLINEABLE setAtomicsMode #-}
{# fun unsafe cublasSetAtomicsMode as setAtomicsMode
  { useHandle `Handle'
  , cFromEnum `AtomicsMode'
  }
  -> `()' checkStatus* #}


-- | Determine whether cuBLAS library functions are allowed to use atomic
-- operations.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublasgetatomicsmode>
--
{-# INLINEABLE getAtomicsMode #-}
{# fun unsafe cublasGetAtomicsMode as getAtomicsMode
  { useHandle `Handle'
  , alloca-   `AtomicsMode' peekEnum*
  }
  -> `()' checkStatus*- #}


-- | Set whether cuBLAS library functions are allowed to use Tensor Core
-- operations where available.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublassetmathmode>
--
-- @since 0.4.0.0@
--
{-# INLINEABLE setMathMode #-}
#if CUDA_VERSION < 9000
setMathMode :: Handle -> MathMode -> IO ()
setMathMode _ _ = requireSDK 'setMathMode 9.0
#else
{# fun unsafe cublasSetMathMode as setMathMode
  { useHandle `Handle'
  , cFromEnum `MathMode'
  }
  -> `()' checkStatus*- #}
#endif


-- | Determine whether cuBLAS library functions are allowed to use Tensor Core
-- operations where available.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublasgetmathmode>
--
-- @since 0.4.0.0@
--
{-# INLINEABLE getMathMode #-}
#if CUDA_VERSION < 9000
getMathMode :: Handle -> IO MathMode
getMathMode _ = requireSDK 'getMathMode 9.0
#else
{# fun unsafe cublasGetMathMode as getMathMode
  { useHandle `Handle'
  , alloca-   `MathMode' peekEnum*
  }
  -> `()' checkStatus*- #}
#endif

