--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Level2
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuBLAS Level-2 function reference:
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-2-function-reference>
--

module Foreign.CUDA.BLAS.Level2 (

  Operation(..),
  Fill(..),
  Diagonal(..),
  sgbmv,
  dgbmv,
  cgbmv,
  zgbmv,
  sgemv,
  dgemv,
  cgemv,
  zgemv,
  sger,
  dger,
  cgerc,
  zgerc,
  cgeru,
  zgeru,
  ssbmv,
  dsbmv,
  sspmv,
  dspmv,
  sspr,
  dspr,
  sspr2,
  dspr2,
  ssymv,
  dsymv,
  csymv,
  zsymv,
  ssyr,
  dsyr,
  csyr,
  zsyr,
  ssyr2,
  dsyr2,
  csyr2,
  zsyr2,
  stbmv,
  dtbmv,
  ctbmv,
  ztbmv,
  stbsv,
  dtbsv,
  ctbsv,
  ztbsv,
  stpmv,
  dtpmv,
  ctpmv,
  ztpmv,
  stpsv,
  dtpsv,
  ctpsv,
  ztpsv,
  strmv,
  dtrmv,
  ctrmv,
  ztrmv,
  strsv,
  dtrsv,
  ctrsv,
  ztrsv,
  chemv,
  zhemv,
  chbmv,
  zhbmv,
  chpmv,
  zhpmv,
  cher,
  zher,
  cher2,
  zher2,
  chpr,
  zhpr,
  chpr2,
  zhpr2,

) where

import Data.Complex
import Foreign
import Foreign.Storable.Complex ()
import Foreign.CUDA.Ptr
import Foreign.CUDA.BLAS.Internal.C2HS
import Foreign.CUDA.BLAS.Internal.Types

#include "cbits/stubs.h"
{# context lib="cublas" #}

{-# INLINE useDevP #-}
useDevP :: DevicePtr a -> Ptr b
useDevP = useDevicePtr . castDevPtr


{-# INLINEABLE sgbmv #-}
{# fun unsafe cublasSgbmv_v2 as sgbmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgbmv #-}
{# fun unsafe cublasDgbmv_v2 as dgbmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgbmv #-}
{# fun unsafe cublasCgbmv_v2 as cgbmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgbmv #-}
{# fun unsafe cublasZgbmv_v2 as zgbmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgemv #-}
{# fun unsafe cublasSgemv_v2 as sgemv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgemv #-}
{# fun unsafe cublasDgemv_v2 as dgemv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemv #-}
{# fun unsafe cublasCgemv_v2 as cgemv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemv #-}
{# fun unsafe cublasZgemv_v2 as zgemv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sger #-}
{# fun unsafe cublasSger_v2 as sger { useHandle `Handle', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dger #-}
{# fun unsafe cublasDger_v2 as dger { useHandle `Handle', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgerc #-}
{# fun unsafe cublasCgerc_v2 as cgerc { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgerc #-}
{# fun unsafe cublasZgerc_v2 as zgerc { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgeru #-}
{# fun unsafe cublasCgeru_v2 as cgeru { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgeru #-}
{# fun unsafe cublasZgeru_v2 as zgeru { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssbmv #-}
{# fun unsafe cublasSsbmv_v2 as ssbmv { useHandle `Handle', cFromEnum `Fill', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsbmv #-}
{# fun unsafe cublasDsbmv_v2 as dsbmv { useHandle `Handle', cFromEnum `Fill', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sspmv #-}
{# fun unsafe cublasSspmv_v2 as sspmv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dspmv #-}
{# fun unsafe cublasDspmv_v2 as dspmv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sspr #-}
{# fun unsafe cublasSspr_v2 as sspr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dspr #-}
{# fun unsafe cublasDspr_v2 as dspr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double' } -> `()' checkStatus* #}

{-# INLINEABLE sspr2 #-}
{# fun unsafe cublasSspr2_v2 as sspr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dspr2 #-}
{# fun unsafe cublasDspr2_v2 as dspr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double' } -> `()' checkStatus* #}

{-# INLINEABLE ssymv #-}
{# fun unsafe cublasSsymv_v2 as ssymv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsymv #-}
{# fun unsafe cublasDsymv_v2 as dsymv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csymv #-}
{# fun unsafe cublasCsymv_v2 as csymv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsymv #-}
{# fun unsafe cublasZsymv_v2 as zsymv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssyr #-}
{# fun unsafe cublasSsyr_v2 as ssyr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsyr #-}
{# fun unsafe cublasDsyr_v2 as dsyr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyr #-}
{# fun unsafe cublasCsyr_v2 as csyr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsyr #-}
{# fun unsafe cublasZsyr_v2 as zsyr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssyr2 #-}
{# fun unsafe cublasSsyr2_v2 as ssyr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsyr2 #-}
{# fun unsafe cublasDsyr2_v2 as dsyr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyr2 #-}
{# fun unsafe cublasCsyr2_v2 as csyr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsyr2 #-}
{# fun unsafe cublasZsyr2_v2 as zsyr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE stbmv #-}
{# fun unsafe cublasStbmv_v2 as stbmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtbmv #-}
{# fun unsafe cublasDtbmv_v2 as dtbmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctbmv #-}
{# fun unsafe cublasCtbmv_v2 as ctbmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztbmv #-}
{# fun unsafe cublasZtbmv_v2 as ztbmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE stbsv #-}
{# fun unsafe cublasStbsv_v2 as stbsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtbsv #-}
{# fun unsafe cublasDtbsv_v2 as dtbsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctbsv #-}
{# fun unsafe cublasCtbsv_v2 as ctbsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztbsv #-}
{# fun unsafe cublasZtbsv_v2 as ztbsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE stpmv #-}
{# fun unsafe cublasStpmv_v2 as stpmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtpmv #-}
{# fun unsafe cublasDtpmv_v2 as dtpmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctpmv #-}
{# fun unsafe cublasCtpmv_v2 as ctpmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztpmv #-}
{# fun unsafe cublasZtpmv_v2 as ztpmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE stpsv #-}
{# fun unsafe cublasStpsv_v2 as stpsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtpsv #-}
{# fun unsafe cublasDtpsv_v2 as dtpsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctpsv #-}
{# fun unsafe cublasCtpsv_v2 as ctpsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztpsv #-}
{# fun unsafe cublasZtpsv_v2 as ztpsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strmv #-}
{# fun unsafe cublasStrmv_v2 as strmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtrmv #-}
{# fun unsafe cublasDtrmv_v2 as dtrmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctrmv #-}
{# fun unsafe cublasCtrmv_v2 as ctrmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztrmv #-}
{# fun unsafe cublasZtrmv_v2 as ztrmv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strsv #-}
{# fun unsafe cublasStrsv_v2 as strsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtrsv #-}
{# fun unsafe cublasDtrsv_v2 as dtrsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctrsv #-}
{# fun unsafe cublasCtrsv_v2 as ctrsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztrsv #-}
{# fun unsafe cublasZtrsv_v2 as ztrsv { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chemv #-}
{# fun unsafe cublasChemv_v2 as chemv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zhemv #-}
{# fun unsafe cublasZhemv_v2 as zhemv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chbmv #-}
{# fun unsafe cublasChbmv_v2 as chbmv { useHandle `Handle', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zhbmv #-}
{# fun unsafe cublasZhbmv_v2 as zhbmv { useHandle `Handle', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chpmv #-}
{# fun unsafe cublasChpmv_v2 as chpmv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zhpmv #-}
{# fun unsafe cublasZhpmv_v2 as zhpmv { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cher #-}
{# fun unsafe cublasCher_v2 as cher { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zher #-}
{# fun unsafe cublasZher_v2 as zher { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cher2 #-}
{# fun unsafe cublasCher2_v2 as cher2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zher2 #-}
{# fun unsafe cublasZher2_v2 as zher2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chpr #-}
{# fun unsafe cublasChpr_v2 as chpr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zhpr #-}
{# fun unsafe cublasZhpr_v2 as zhpr { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE chpr2 #-}
{# fun unsafe cublasChpr2_v2 as chpr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zhpr2 #-}
{# fun unsafe cublasZhpr2_v2 as zhpr2 { useHandle `Handle', cFromEnum `Fill', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus* #}
