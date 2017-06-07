{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Stream
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Stream (

  setStream,

) where

import Foreign.CUDA.Driver.Stream
import Foreign.CUDA.BLAS.Internal.C2HS
import Foreign.CUDA.BLAS.Internal.Types

#include "cbits/stubs.h"
{# context lib="cublas" #}


-- | Sets the execution stream which all subsequent cuBLAS library functions
-- will execute with. If not set, functions execute in the default stream (which
-- never overlaps any other operations).
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublassetstream>
--
{-# INLINEABLE setStream #-}
{# fun unsafe cublasSetStream_v2 as setStream
  { useHandle `Handle'
  , useStream `Stream'
  }
  -> `()' checkStatus* #}

