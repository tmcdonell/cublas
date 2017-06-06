{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.BLAS.Error
  where

-- System
import Data.Typeable
import Control.Exception

import Foreign.C.Types
import Foreign.CUDA.BLAS.Internal.C2HS

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
#if CUDA_VERSION >= 8000
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


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
resultIfOk :: (Status, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)


-- | Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)

checkStatus :: CInt -> IO ()
checkStatus = nothingIfOk . cToEnum

