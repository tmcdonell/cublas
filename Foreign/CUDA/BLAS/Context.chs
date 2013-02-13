{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.BLAS.Context (

  -- * Context
  Handle(..),
  create, destroy,

) where

-- Friends
import Foreign.CUDA.BLAS.Error
import Foreign.CUDA.BLAS.Internal.C2HS

-- System
import Foreign
import Foreign.C
import Control.Monad                            ( liftM )

#include <cublas_v2.h>
{# context lib="cublas" #}


-- | Operations handle
--
newtype Handle = Handle { useHandle :: {# type cublasHandle_t #}}


-- Context management ----------------------------------------------------------
--

-- | This function initializes the CUBLAS library and creates a handle to an
-- opaque structure holding the CUBLAS library context. It allocates hardware
-- resources on the host and device and must be called prior to making any other
-- CUBLAS library calls.
--
create :: IO Handle
create = resultIfOk =<< cublasCreate

{# fun unsafe cublasCreate_v2 as cublasCreate
  { alloca- `Handle' peekHdl* } -> `Status' cToEnum #}
  where
    peekHdl = liftM Handle . peek


-- | This function releases hardware resources used by the CUBLAS library. The
-- release of GPU resources may be deferred until the application exits. This
-- function is usually the last call with a particular handle to the CUBLAS
-- library.
--
destroy :: Handle -> IO ()
destroy ctx = nothingIfOk =<< cublasDestroy ctx

{# fun unsafe cublasDestroy_v2 as cublasDestroy
  { useHandle `Handle' } -> `Status' cToEnum #}

