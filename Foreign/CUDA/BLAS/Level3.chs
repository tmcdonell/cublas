--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  Type(..),
  GemmAlgorithm(..),
  sgemm,
  dgemm,
  cgemm,
  zgemm,
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
  sgeam,
  dgeam,
  cgeam,
  zgeam,
  sdgmm,
  ddgmm,
  cdgmm,
  zdgmm,
  sgetrfBatched,
  dgetrfBatched,
  cgetrfBatched,
  zgetrfBatched,
  sgetriBatched,
  dgetriBatched,
  cgetriBatched,
  zgetriBatched,
  smatinvBatched,
  dmatinvBatched,
  cmatinvBatched,
  zmatinvBatched,
  sgeqrfBatched,
  dgeqrfBatched,
  cgeqrfBatched,
  zgeqrfBatched,
  sgelsBatched,
  dgelsBatched,
  cgelsBatched,
  zgelsBatched,
  stpttr,
  dtpttr,
  ctpttr,
  ztpttr,
  strttp,
  dtrttp,
  ctrttp,
  ztrttp,
  sgetrsBatched,
  dgetrsBatched,
  cgetrsBatched,
  zgetrsBatched,
  hgemm,
  sgemmEx,
  cgemm3m,
  zgemm3m,
  sgemmStridedBatched,
  dgemmStridedBatched,
  cgemmStridedBatched,
  zgemmStridedBatched,
  hgemmStridedBatched,
  cgemm3mStridedBatched,
  cgemmEx,
  gemmEx,
  csyrkEx,
  csyrk3mEx,
  cherkEx,
  cherk3mEx,
  nrm2Ex,
  axpyEx,
  dotEx,
  dotcEx,
  scalEx,
  gemmBatchedEx,
  gemmStridedBatchedEx,

) where

import Data.Complex
import Numeric.Half
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Foreign.CUDA.Ptr
import Foreign.CUDA.BLAS.Error
import Foreign.CUDA.BLAS.Internal.C2HS
import Foreign.CUDA.BLAS.Internal.Types

#include "cbits/stubs.h"
{# context lib="cublas" #}

{-# INLINE useDevP #-}
useDevP :: DevicePtr a -> Ptr b
useDevP = useDevicePtr . castDevPtr

{-# INLINE useHostP #-}
useHostP :: HostPtr a -> Ptr b
useHostP = useHostPtr . castHostPtr


{-# INLINEABLE sgemm #-}
{# fun unsafe cublasSgemm_v2 as sgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgemm #-}
{# fun unsafe cublasDgemm_v2 as dgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemm #-}
{# fun unsafe cublasCgemm_v2 as cgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemm #-}
{# fun unsafe cublasZgemm_v2 as zgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

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

{-# INLINEABLE sgeam #-}
{# fun unsafe cublasSgeam as sgeam { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgeam #-}
{# fun unsafe cublasDgeam as dgeam { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgeam #-}
{# fun unsafe cublasCgeam as cgeam { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgeam #-}
{# fun unsafe cublasZgeam as zgeam { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sdgmm #-}
{# fun unsafe cublasSdgmm as sdgmm { useHandle `Handle', cFromEnum `Side', `Int', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ddgmm #-}
{# fun unsafe cublasDdgmm as ddgmm { useHandle `Handle', cFromEnum `Side', `Int', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cdgmm #-}
{# fun unsafe cublasCdgmm as cdgmm { useHandle `Handle', cFromEnum `Side', `Int', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zdgmm #-}
{# fun unsafe cublasZdgmm as zdgmm { useHandle `Handle', cFromEnum `Side', `Int', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgetrfBatched #-}
{# fun unsafe cublasSgetrfBatched as sgetrfBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', castPtr `Ptr Int32', castPtr `Ptr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgetrfBatched #-}
{# fun unsafe cublasDgetrfBatched as dgetrfBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', castPtr `Ptr Int32', castPtr `Ptr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgetrfBatched #-}
{# fun unsafe cublasCgetrfBatched as cgetrfBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', castPtr `Ptr Int32', castPtr `Ptr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgetrfBatched #-}
{# fun unsafe cublasZgetrfBatched as zgetrfBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', castPtr `Ptr Int32', castPtr `Ptr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgetriBatched #-}
{# fun unsafe cublasSgetriBatched as sgetriBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgetriBatched #-}
{# fun unsafe cublasDgetriBatched as dgetriBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgetriBatched #-}
{# fun unsafe cublasCgetriBatched as cgetriBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgetriBatched #-}
{# fun unsafe cublasZgetriBatched as zgetriBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE smatinvBatched #-}
{# fun unsafe cublasSmatinvBatched as smatinvBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dmatinvBatched #-}
{# fun unsafe cublasDmatinvBatched as dmatinvBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cmatinvBatched #-}
{# fun unsafe cublasCmatinvBatched as cmatinvBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zmatinvBatched #-}
{# fun unsafe cublasZmatinvBatched as zmatinvBatched { useHandle `Handle', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgeqrfBatched #-}
{# fun unsafe cublasSgeqrfBatched as sgeqrfBatched { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr (DevicePtr Float)', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgeqrfBatched #-}
{# fun unsafe cublasDgeqrfBatched as dgeqrfBatched { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr (DevicePtr Double)', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgeqrfBatched #-}
{# fun unsafe cublasCgeqrfBatched as cgeqrfBatched { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgeqrfBatched #-}
{# fun unsafe cublasZgeqrfBatched as zgeqrfBatched { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgelsBatched #-}
{# fun unsafe cublasSgelsBatched as sgelsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useHostP `HostPtr Int32', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgelsBatched #-}
{# fun unsafe cublasDgelsBatched as dgelsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useHostP `HostPtr Int32', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgelsBatched #-}
{# fun unsafe cublasCgelsBatched as cgelsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useHostP `HostPtr Int32', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgelsBatched #-}
{# fun unsafe cublasZgelsBatched as zgelsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useHostP `HostPtr Int32', useDevP `DevicePtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE stpttr #-}
{# fun unsafe cublasStpttr as stpttr { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dtpttr #-}
{# fun unsafe cublasDtpttr as dtpttr { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ctpttr #-}
{# fun unsafe cublasCtpttr as ctpttr { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ztpttr #-}
{# fun unsafe cublasZtpttr as ztpttr { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE strttp #-}
{# fun unsafe cublasStrttp as strttp { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dtrttp #-}
{# fun unsafe cublasDtrttp as dtrttp { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double' } -> `()' checkStatus* #}

{-# INLINEABLE ctrttp #-}
{# fun unsafe cublasCtrttp as ctrttp { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE ztrttp #-}
{# fun unsafe cublasZtrttp as ztrttp { useHandle `Handle', cFromEnum `Fill', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus* #}
#if CUDA_VERSION >= 7000

{-# INLINEABLE sgetrsBatched #-}
{# fun unsafe cublasSgetrsBatched as sgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Float)', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgetrsBatched #-}
{# fun unsafe cublasDgetrsBatched as dgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Double)', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgetrsBatched #-}
{# fun unsafe cublasCgetrsBatched as cgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgetrsBatched #-}
{# fun unsafe cublasZgetrsBatched as zgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}
#else

sgetrsBatched :: Handle -> Operation -> Int -> Int -> DevicePtr (DevicePtr Float) -> Int -> DevicePtr Int32 -> DevicePtr (DevicePtr Float) -> Int -> HostPtr Int32 -> Int -> IO ()
sgetrsBatched _ _ _ _ _ _ _ _ _ _ _ = cublasError "'sgetrsBatched' requires at least cuda-7.0"

dgetrsBatched :: Handle -> Operation -> Int -> Int -> DevicePtr (DevicePtr Double) -> Int -> DevicePtr Int32 -> DevicePtr (DevicePtr Double) -> Int -> HostPtr Int32 -> Int -> IO ()
dgetrsBatched _ _ _ _ _ _ _ _ _ _ _ = cublasError "'dgetrsBatched' requires at least cuda-7.0"

cgetrsBatched :: Handle -> Operation -> Int -> Int -> DevicePtr (DevicePtr (Complex Float)) -> Int -> DevicePtr Int32 -> DevicePtr (DevicePtr (Complex Float)) -> Int -> HostPtr Int32 -> Int -> IO ()
cgetrsBatched _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cgetrsBatched' requires at least cuda-7.0"

zgetrsBatched :: Handle -> Operation -> Int -> Int -> DevicePtr (DevicePtr (Complex Double)) -> Int -> DevicePtr Int32 -> DevicePtr (DevicePtr (Complex Double)) -> Int -> HostPtr Int32 -> Int -> IO ()
zgetrsBatched _ _ _ _ _ _ _ _ _ _ _ = cublasError "'zgetrsBatched' requires at least cuda-7.0"
#endif
#if CUDA_VERSION >= 7500

{-# INLINEABLE hgemm #-}
{# fun unsafe cublasHgemm as hgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int', useDevP `DevicePtr Half', `Int', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgemmEx #-}
{# fun unsafe cublasSgemmEx as sgemmEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int' } -> `()' checkStatus* #}
#else

hgemm :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr Half -> DevicePtr Half -> Int -> DevicePtr Half -> Int -> Ptr Half -> DevicePtr Half -> Int -> IO ()
hgemm _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'hgemm' requires at least cuda-7.5"

sgemmEx :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> IO ()
sgemmEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'sgemmEx' requires at least cuda-7.5"
#endif
#if CUDA_VERSION >= 8000

{-# INLINEABLE cgemm3m #-}
{# fun unsafe cublasCgemm3m as cgemm3m { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemm3m #-}
{# fun unsafe cublasZgemm3m as zgemm3m { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgemmStridedBatched #-}
{# fun unsafe cublasSgemmStridedBatched as sgemmStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', `Int64', useDevP `DevicePtr Float', `Int', `Int64', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgemmStridedBatched #-}
{# fun unsafe cublasDgemmStridedBatched as dgemmStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', `Int64', useDevP `DevicePtr Double', `Int', `Int64', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemmStridedBatched #-}
{# fun unsafe cublasCgemmStridedBatched as cgemmStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int64', useDevP `DevicePtr (Complex Float)', `Int', `Int64', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgemmStridedBatched #-}
{# fun unsafe cublasZgemmStridedBatched as zgemmStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int64', useDevP `DevicePtr (Complex Double)', `Int', `Int64', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE hgemmStridedBatched #-}
{# fun unsafe cublasHgemmStridedBatched as hgemmStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int', `Int64', useDevP `DevicePtr Half', `Int', `Int64', castPtr `Ptr Half', useDevP `DevicePtr Half', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemm3mStridedBatched #-}
{# fun unsafe cublasCgemm3mStridedBatched as cgemm3mStridedBatched { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int64', useDevP `DevicePtr (Complex Float)', `Int', `Int64', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int64', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgemmEx #-}
{# fun unsafe cublasCgemmEx as cgemmEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr ()', cFromEnum `Type', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE gemmEx #-}
{# fun unsafe cublasGemmEx as gemmEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr ()', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr ()', useDevP `DevicePtr ()', cFromEnum `Type', `Int', cFromEnum `Type', cFromEnum `GemmAlgorithm' } -> `()' checkStatus* #}

{-# INLINEABLE csyrkEx #-}
{# fun unsafe cublasCsyrkEx as csyrkEx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', cFromEnum `Type', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csyrk3mEx #-}
{# fun unsafe cublasCsyrk3mEx as csyrk3mEx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', cFromEnum `Type', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cherkEx #-}
{# fun unsafe cublasCherkEx as cherkEx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', cFromEnum `Type', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cherk3mEx #-}
{# fun unsafe cublasCherk3mEx as cherk3mEx { useHandle `Handle', cFromEnum `Fill', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', cFromEnum `Type', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE nrm2Ex #-}
{# fun unsafe cublasNrm2Ex as nrm2Ex { useHandle `Handle', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr ()', cFromEnum `Type', cFromEnum `Type' } -> `()' checkStatus* #}

{-# INLINEABLE axpyEx #-}
{# fun unsafe cublasAxpyEx as axpyEx { useHandle `Handle', `Int', castPtr `Ptr ()', cFromEnum `Type', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', cFromEnum `Type' } -> `()' checkStatus* #}

{-# INLINEABLE dotEx #-}
{# fun unsafe cublasDotEx as dotEx { useHandle `Handle', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr ()', cFromEnum `Type', cFromEnum `Type' } -> `()' checkStatus* #}

{-# INLINEABLE dotcEx #-}
{# fun unsafe cublasDotcEx as dotcEx { useHandle `Handle', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr ()', cFromEnum `Type', cFromEnum `Type' } -> `()' checkStatus* #}

{-# INLINEABLE scalEx #-}
{# fun unsafe cublasScalEx as scalEx { useHandle `Handle', `Int', castPtr `Ptr ()', cFromEnum `Type', useDevP `DevicePtr ()', cFromEnum `Type', `Int', cFromEnum `Type' } -> `()' checkStatus* #}
#else

cgemm3m :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr (Complex Float) -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> IO ()
cgemm3m _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cgemm3m' requires at least cuda-8.0"

zgemm3m :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr (Complex Double) -> Int -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> IO ()
zgemm3m _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'zgemm3m' requires at least cuda-8.0"

sgemmStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr Float -> DevicePtr Float -> Int -> Int64 -> DevicePtr Float -> Int -> Int64 -> Ptr Float -> DevicePtr Float -> Int -> Int64 -> Int -> IO ()
sgemmStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'sgemmStridedBatched' requires at least cuda-8.0"

dgemmStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr Double -> DevicePtr Double -> Int -> Int64 -> DevicePtr Double -> Int -> Int64 -> Ptr Double -> DevicePtr Double -> Int -> Int64 -> Int -> IO ()
dgemmStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'dgemmStridedBatched' requires at least cuda-8.0"

cgemmStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int64 -> DevicePtr (Complex Float) -> Int -> Int64 -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int64 -> Int -> IO ()
cgemmStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cgemmStridedBatched' requires at least cuda-8.0"

zgemmStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int64 -> DevicePtr (Complex Double) -> Int -> Int64 -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int64 -> Int -> IO ()
zgemmStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'zgemmStridedBatched' requires at least cuda-8.0"

hgemmStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr Half -> DevicePtr Half -> Int -> Int64 -> DevicePtr Half -> Int -> Int64 -> Ptr Half -> DevicePtr Half -> Int -> Int64 -> Int -> IO ()
hgemmStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'hgemmStridedBatched' requires at least cuda-8.0"

cgemm3mStridedBatched :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int64 -> DevicePtr (Complex Float) -> Int -> Int64 -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int64 -> Int -> IO ()
cgemm3mStridedBatched _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cgemm3mStridedBatched' requires at least cuda-8.0"

cgemmEx :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr (Complex Float) -> DevicePtr () -> Type -> Int -> IO ()
cgemmEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cgemmEx' requires at least cuda-8.0"

gemmEx :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr () -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr () -> DevicePtr () -> Type -> Int -> Type -> GemmAlgorithm -> IO ()
gemmEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'gemmEx' requires at least cuda-8.0"

csyrkEx :: Handle -> Fill -> Operation -> Int -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> Ptr Float -> DevicePtr (Complex Float) -> Type -> Int -> IO ()
csyrkEx _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'csyrkEx' requires at least cuda-8.0"

csyrk3mEx :: Handle -> Fill -> Operation -> Int -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> Ptr Float -> DevicePtr (Complex Float) -> Type -> Int -> IO ()
csyrk3mEx _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'csyrk3mEx' requires at least cuda-8.0"

cherkEx :: Handle -> Fill -> Operation -> Int -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> Ptr Float -> DevicePtr (Complex Float) -> Type -> Int -> IO ()
cherkEx _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cherkEx' requires at least cuda-8.0"

cherk3mEx :: Handle -> Fill -> Operation -> Int -> Int -> Ptr Float -> DevicePtr () -> Type -> Int -> Ptr Float -> DevicePtr (Complex Float) -> Type -> Int -> IO ()
cherk3mEx _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'cherk3mEx' requires at least cuda-8.0"

nrm2Ex :: Handle -> Int -> DevicePtr () -> Type -> Int -> Ptr () -> Type -> Type -> IO ()
nrm2Ex _ _ _ _ _ _ _ _ = cublasError "'nrm2Ex' requires at least cuda-8.0"

axpyEx :: Handle -> Int -> Ptr () -> Type -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Type -> IO ()
axpyEx _ _ _ _ _ _ _ _ _ _ _ = cublasError "'axpyEx' requires at least cuda-8.0"

dotEx :: Handle -> Int -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr () -> Type -> Type -> IO ()
dotEx _ _ _ _ _ _ _ _ _ _ _ = cublasError "'dotEx' requires at least cuda-8.0"

dotcEx :: Handle -> Int -> DevicePtr () -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr () -> Type -> Type -> IO ()
dotcEx _ _ _ _ _ _ _ _ _ _ _ = cublasError "'dotcEx' requires at least cuda-8.0"

scalEx :: Handle -> Int -> Ptr () -> Type -> DevicePtr () -> Type -> Int -> Type -> IO ()
scalEx _ _ _ _ _ _ _ _ = cublasError "'scalEx' requires at least cuda-8.0"
#endif
#if CUDA_VERSION >= 9100

{-# INLINEABLE gemmBatchedEx #-}
{# fun unsafe cublasGemmBatchedEx as gemmBatchedEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr ()', useDevP `DevicePtr (DevicePtr ())', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr ()', useDevP `DevicePtr ()', cFromEnum `Type', `Int', `Int', cFromEnum `Type', cFromEnum `GemmAlgorithm' } -> `()' checkStatus* #}

{-# INLINEABLE gemmStridedBatchedEx #-}
{# fun unsafe cublasGemmStridedBatchedEx as gemmStridedBatchedEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr ()', useDevP `DevicePtr ()', cFromEnum `Type', `Int', `Int64', useDevP `DevicePtr ()', cFromEnum `Type', `Int', `Int64', castPtr `Ptr ()', useDevP `DevicePtr ()', cFromEnum `Type', `Int', `Int64', `Int', cFromEnum `Type', cFromEnum `GemmAlgorithm' } -> `()' checkStatus* #}
#else

gemmBatchedEx :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr () -> DevicePtr (DevicePtr ()) -> Type -> Int -> DevicePtr () -> Type -> Int -> Ptr () -> DevicePtr () -> Type -> Int -> Int -> Type -> GemmAlgorithm -> IO ()
gemmBatchedEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'gemmBatchedEx' requires at least cuda-9.1"

gemmStridedBatchedEx :: Handle -> Operation -> Operation -> Int -> Int -> Int -> Ptr () -> DevicePtr () -> Type -> Int -> Int64 -> DevicePtr () -> Type -> Int -> Int64 -> Ptr () -> DevicePtr () -> Type -> Int -> Int64 -> Int -> Type -> GemmAlgorithm -> IO ()
gemmStridedBatchedEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cublasError "'gemmStridedBatchedEx' requires at least cuda-9.1"
#endif
