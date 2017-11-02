{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Error
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Error
  where

-- friends
import Foreign.CUDA.BLAS.Internal.C2HS

-- system
import Control.Exception
import Data.Typeable
import Foreign.C.Types
import Language.Haskell.TH
import Text.Printf

#include "cbits/stubs.h"
{# context lib="cublas" #}


-- | Error codes used by cuBLAS library functions
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublasstatus_t>
--
{# enum cublasStatus_t as Status
  { underscoreToCase }
  with prefix="CUBLAS_STATUS" deriving (Eq, Show) #}

-- Describe each error code
--
describe :: Status -> String
describe Success         = "success"
describe NotInitialized  = "library not initialised"
describe AllocFailed     = "resource allocation failed"
describe InvalidValue    = "unsupported value or parameter passed to a function"
describe ArchMismatch    = "unsupported on current architecture"
describe MappingError    = "access to GPU memory failed"
describe ExecutionFailed = "execution failed"
describe InternalError   = "internal error"
describe NotSupported    = "not supported"
#if CUDA_VERSION >= 6500
describe LicenseError    = "license error"
#endif


-- Exceptions ------------------------------------------------------------------
--
data CUBLASException
  = ExitCode  Status
  | UserError String
  deriving Typeable

instance Exception CUBLASException

instance Show CUBLASException where
  showsPrec _ (ExitCode  s) = showString ("CUBLAS Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUBLAS Exception: " ++ s)


-- | Raise a CUBLASException in the IO Monad
--
cublasError :: String -> IO a
cublasError s = throwIO (UserError s)

-- |
-- A specially formatted error message
--
requireSDK :: Name -> Double -> IO a
requireSDK n v = cublasError $ printf "'%s' requires at least cuda-%3.1f\n" (show n) v


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
{-# INLINE resultIfOk #-}
resultIfOk :: (Status, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)


-- | Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
{-# INLINE nothingIfOk #-}
nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)

{-# INLINE checkStatus #-}
checkStatus :: CInt -> IO ()
checkStatus = nothingIfOk . cToEnum

