--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuBLAS Level-3 function reference:
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-3-function-reference>
--

module Foreign.CUDA.BLAS.Level3 (

  Operation(..),
  Fill(..),
  Diagonal(..),
  Side(..),
  sgemm,
  dgemm,
  cgemm,
  zgemm,
  hgemm,
  sgemmBatched,
  dgemmBatched,
  cgemmBatched,
  zgemmBatched,
  ssymm,
  dsymm,
  csymm,
  zsymm,
  ssyrk,
  dsyrk,
  csyrk,
  zsyrk,
  ssyr2k,
  dsyr2k,
  csyr2k,
  zsyr2k,
  ssyrkx,
  dsyrkx,
  csyrkx,
  zsyrkx,
  strmm,
  dtrmm,
  ctrmm,
  ztrmm,
  strsm,
  dtrsm,
  ctrsm,
  ztrsm,
  strsmBatched,
  dtrsmBatched,
  ctrsmBatched,
  ztrsmBatched,
  chemm,
  zhemm,
  cherk,
  zherk,
  cher2k,
  zher2k,
  cherkx,
  zherkx,

) where

import Data.Complex
import Numeric.Half
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


{-# INLINEABLE sgemm #-}
{# fun unsafe cublasSgemm_v2 as sgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgemm #-}
{# fun unsafe cublasDgemm_v2 as dgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemm #-}
{# fun unsafe cublasCgemm_v2 as cgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemm #-}
{# fun unsafe cublasZgemm_v2 as zgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE hgemm #-}
{# fun unsafe cublasHgemm as hgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int', useDevP `DevicePtr Half', `Int', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgemmBatched #-}
{# fun unsafe cublasSgemmBatched as sgemmBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', castPtr `Ptr Float', useDevP `DevicePtr (DevicePtr Float)', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgemmBatched #-}
{# fun unsafe cublasDgemmBatched as dgemmBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', castPtr `Ptr Double', useDevP `DevicePtr (DevicePtr Double)', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemmBatched #-}
{# fun unsafe cublasCgemmBatched as cgemmBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemmBatched #-}
{# fun unsafe cublasZgemmBatched as zgemmBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssymm #-}
{# fun unsafe cublasSsymm_v2 as ssymm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsymm #-}
{# fun unsafe cublasDsymm_v2 as dsymm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csymm #-}
{# fun unsafe cublasCsymm_v2 as csymm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsymm #-}
{# fun unsafe cublasZsymm_v2 as zsymm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssyrk #-}
{# fun unsafe cublasSsyrk_v2 as ssyrk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsyrk #-}
{# fun unsafe cublasDsyrk_v2 as dsyrk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyrk #-}
{# fun unsafe cublasCsyrk_v2 as csyrk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsyrk #-}
{# fun unsafe cublasZsyrk_v2 as zsyrk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssyr2k #-}
{# fun unsafe cublasSsyr2k_v2 as ssyr2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsyr2k #-}
{# fun unsafe cublasDsyr2k_v2 as dsyr2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyr2k #-}
{# fun unsafe cublasCsyr2k_v2 as csyr2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsyr2k #-}
{# fun unsafe cublasZsyr2k_v2 as zsyr2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ssyrkx #-}
{# fun unsafe cublasSsyrkx as ssyrkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dsyrkx #-}
{# fun unsafe cublasDsyrkx as dsyrkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyrkx #-}
{# fun unsafe cublasCsyrkx as csyrkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zsyrkx #-}
{# fun unsafe cublasZsyrkx as zsyrkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strmm #-}
{# fun unsafe cublasStrmm_v2 as strmm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtrmm #-}
{# fun unsafe cublasDtrmm_v2 as dtrmm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctrmm #-}
{# fun unsafe cublasCtrmm_v2 as ctrmm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztrmm #-}
{# fun unsafe cublasZtrmm_v2 as ztrmm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strsm #-}
{# fun unsafe cublasStrsm_v2 as strsm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtrsm #-}
{# fun unsafe cublasDtrsm_v2 as dtrsm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctrsm #-}
{# fun unsafe cublasCtrsm_v2 as ctrsm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztrsm #-}
{# fun unsafe cublasZtrsm_v2 as ztrsm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strsmBatched #-}
{# fun unsafe cublasStrsmBatched as strsmBatched { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtrsmBatched #-}
{# fun unsafe cublasDtrsmBatched as dtrsmBatched { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctrsmBatched #-}
{# fun unsafe cublasCtrsmBatched as ctrsmBatched { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztrsmBatched #-}
{# fun unsafe cublasZtrsmBatched as ztrsmBatched { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', cFromEnum `Operation', cFromEnum `Diagonal', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chemm #-}
{# fun unsafe cublasChemm_v2 as chemm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zhemm #-}
{# fun unsafe cublasZhemm_v2 as zhemm { useHandle `Handle', cFromEnum `Side', cFromEnum `Fill', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cherk #-}
{# fun unsafe cublasCherk_v2 as cherk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zherk #-}
{# fun unsafe cublasZherk_v2 as zherk { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cher2k #-}
{# fun unsafe cublasCher2k_v2 as cher2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zher2k #-}
{# fun unsafe cublasZher2k_v2 as zher2k { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cherkx #-}
{# fun unsafe cublasCherkx as cherkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zherkx #-}
{# fun unsafe cublasZherkx as zherkx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}
