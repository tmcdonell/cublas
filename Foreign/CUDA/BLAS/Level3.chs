--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
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
  sgetrsBatched,
  dgetrsBatched,
  cgetrsBatched,
  zgetrsBatched,
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
  sgemmEx,

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

{-# INLINEABLE sgetrsBatched #-}
{# fun unsafe cublasSgetrsBatched as sgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Float)', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgetrsBatched #-}
{# fun unsafe cublasDgetrsBatched as dgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr Double)', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgetrsBatched #-}
{# fun unsafe cublasCgetrsBatched as cgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Float))', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgetrsBatched #-}
{# fun unsafe cublasZgetrsBatched as zgetrsBatched { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (DevicePtr (Complex Double))', `Int', useHostP `HostPtr Int32', `Int' } -> `()' checkStatus* #}

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

{-# INLINEABLE sgemmEx #-}
{# fun unsafe cublasSgemmEx as sgemmEx { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int', useDevP `DevicePtr ()', cFromEnum `Type', `Int', castPtr `Ptr Float', useDevP `DevicePtr ()', cFromEnum `Type', `Int' } -> `()' checkStatus* #}
