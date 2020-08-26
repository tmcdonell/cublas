--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Level1
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuBLAS Level-1 function reference:
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-1-function-reference>
--

module Foreign.CUDA.BLAS.Level1 (

  isamax,
  idamax,
  icamax,
  izamax,
  isamin,
  idamin,
  icamin,
  izamin,
  sasum,
  dasum,
  scasum,
  dzasum,
  saxpy,
  daxpy,
  caxpy,
  zaxpy,
  scopy,
  dcopy,
  ccopy,
  zcopy,
  sdot,
  ddot,
  cdotu,
  zdotu,
  cdotc,
  zdotc,
  snrm2,
  dnrm2,
  scnrm2,
  dznrm2,
  srot,
  drot,
  crot,
  csrot,
  zrot,
  zdrot,
  srotg,
  drotg,
  crotg,
  zrotg,
  srotm,
  drotm,
  srotmg,
  drotmg,
  sscal,
  dscal,
  cscal,
  csscal,
  zscal,
  zdscal,
  sswap,
  dswap,
  cswap,
  zswap,

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


{-# INLINEABLE isamax #-}
{# fun unsafe cublasIsamax_v2 as isamax { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE idamax #-}
{# fun unsafe cublasIdamax_v2 as idamax { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE icamax #-}
{# fun unsafe cublasIcamax_v2 as icamax { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE izamax #-}
{# fun unsafe cublasIzamax_v2 as izamax { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE isamin #-}
{# fun unsafe cublasIsamin_v2 as isamin { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE idamin #-}
{# fun unsafe cublasIdamin_v2 as idamin { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE icamin #-}
{# fun unsafe cublasIcamin_v2 as icamin { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE izamin #-}
{# fun unsafe cublasIzamin_v2 as izamin { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sasum #-}
{# fun unsafe cublasSasum_v2 as sasum { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dasum #-}
{# fun unsafe cublasDasum_v2 as dasum { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE scasum #-}
{# fun unsafe cublasScasum_v2 as scasum { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dzasum #-}
{# fun unsafe cublasDzasum_v2 as dzasum { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE saxpy #-}
{# fun unsafe cublasSaxpy_v2 as saxpy { useHandle `Handle', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE daxpy #-}
{# fun unsafe cublasDaxpy_v2 as daxpy { useHandle `Handle', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE caxpy #-}
{# fun unsafe cublasCaxpy_v2 as caxpy { useHandle `Handle', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zaxpy #-}
{# fun unsafe cublasZaxpy_v2 as zaxpy { useHandle `Handle', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE scopy #-}
{# fun unsafe cublasScopy_v2 as scopy { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcopy #-}
{# fun unsafe cublasDcopy_v2 as dcopy { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccopy #-}
{# fun unsafe cublasCcopy_v2 as ccopy { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcopy #-}
{# fun unsafe cublasZcopy_v2 as zcopy { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sdot #-}
{# fun unsafe cublasSdot_v2 as sdot { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE ddot #-}
{# fun unsafe cublasDdot_v2 as ddot { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE cdotu #-}
{# fun unsafe cublasCdotu_v2 as cdotu { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zdotu #-}
{# fun unsafe cublasZdotu_v2 as zdotu { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE cdotc #-}
{# fun unsafe cublasCdotc_v2 as cdotc { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zdotc #-}
{# fun unsafe cublasZdotc_v2 as zdotc { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE snrm2 #-}
{# fun unsafe cublasSnrm2_v2 as snrm2 { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dnrm2 #-}
{# fun unsafe cublasDnrm2_v2 as dnrm2 { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE scnrm2 #-}
{# fun unsafe cublasScnrm2_v2 as scnrm2 { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dznrm2 #-}
{# fun unsafe cublasDznrm2_v2 as dznrm2 { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE srot #-}
{# fun unsafe cublasSrot_v2 as srot { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE drot #-}
{# fun unsafe cublasDrot_v2 as drot { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE crot #-}
{# fun unsafe cublasCrot_v2 as crot { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE csrot #-}
{# fun unsafe cublasCsrot_v2 as csrot { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr Float', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE zrot #-}
{# fun unsafe cublasZrot_v2 as zrot { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE zdrot #-}
{# fun unsafe cublasZdrot_v2 as zdrot { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE srotg #-}
{# fun unsafe cublasSrotg_v2 as srotg { useHandle `Handle', castPtr `Ptr Float', castPtr `Ptr Float', castPtr `Ptr Float', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE drotg #-}
{# fun unsafe cublasDrotg_v2 as drotg { useHandle `Handle', castPtr `Ptr Double', castPtr `Ptr Double', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE crotg #-}
{# fun unsafe cublasCrotg_v2 as crotg { useHandle `Handle', castPtr `Ptr (Complex Float)', castPtr `Ptr (Complex Float)', castPtr `Ptr (Complex Float)', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zrotg #-}
{# fun unsafe cublasZrotg_v2 as zrotg { useHandle `Handle', castPtr `Ptr (Complex Double)', castPtr `Ptr (Complex Double)', castPtr `Ptr (Complex Double)', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE srotm #-}
{# fun unsafe cublasSrotm_v2 as srotm { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE drotm #-}
{# fun unsafe cublasDrotm_v2 as drotm { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE srotmg #-}
{# fun unsafe cublasSrotmg_v2 as srotmg { useHandle `Handle', castPtr `Ptr Float', castPtr `Ptr Float', castPtr `Ptr Float', castPtr `Ptr Float', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE drotmg #-}
{# fun unsafe cublasDrotmg_v2 as drotmg { useHandle `Handle', castPtr `Ptr Double', castPtr `Ptr Double', castPtr `Ptr Double', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE sscal #-}
{# fun unsafe cublasSscal_v2 as sscal { useHandle `Handle', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dscal #-}
{# fun unsafe cublasDscal_v2 as dscal { useHandle `Handle', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cscal #-}
{# fun unsafe cublasCscal_v2 as cscal { useHandle `Handle', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE csscal #-}
{# fun unsafe cublasCsscal_v2 as csscal { useHandle `Handle', `Int', castPtr `Ptr Float', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zscal #-}
{# fun unsafe cublasZscal_v2 as zscal { useHandle `Handle', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zdscal #-}
{# fun unsafe cublasZdscal_v2 as zdscal { useHandle `Handle', `Int', castPtr `Ptr Double', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sswap #-}
{# fun unsafe cublasSswap_v2 as sswap { useHandle `Handle', `Int', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dswap #-}
{# fun unsafe cublasDswap_v2 as dswap { useHandle `Handle', `Int', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cswap #-}
{# fun unsafe cublasCswap_v2 as cswap { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zswap #-}
{# fun unsafe cublasZswap_v2 as zswap { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}
