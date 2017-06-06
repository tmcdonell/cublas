{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.BLAS.Context (

  -- * Context management
  Handle(..),
  create, destroy,

  -- ** Utilities
  PointerMode(..), AtomicsMode(..),
  setPointerMode,
  getPointerMode,
  setAtomicsMode,
  getAtomicsMode,

) where

-- Friends
import Foreign.CUDA.BLAS.Types
import Foreign.CUDA.BLAS.Error
import Foreign.CUDA.BLAS.Internal.C2HS

-- System
import Foreign
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
create :: IO Handle
create = resultIfOk =<< cublasCreate_v2
  where
    {# fun unsafe cublasCreate_v2
      { alloca- `Handle' peekHdl* } -> `Status' cToEnum #}
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


-- | Set the pointer mode used by cuBLAS library functions. The _default_ mode
-- is for values to be passed by reference from the host.
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
getPointerMode :: Handle -> IO PointerMode
getPointerMode h = resultIfOk =<< cublasGetPointerMode_v2 h
  where
    {# fun unsafe cublasGetPointerMode_v2
      { useHandle `Handle'
      , alloca-   `PointerMode' peekPM*
      }
      -> `Status' cToEnum #}

    peekPM = liftM cToEnum . peek


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
getAtomicsMode :: Handle -> IO AtomicsMode
getAtomicsMode h = resultIfOk =<< cublasGetAtomicsMode h
  where
    {# fun unsafe cublasGetAtomicsMode
      { useHandle `Handle'
      , alloca-   `AtomicsMode' peekAM*
      }
      -> `Status' cToEnum #}

    peekAM = liftM cToEnum . peek

