-- |
-- Module      : Foreign.CUDA.BLAS
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The cuBLAS library is an implementation of BLAS (Basic Linear Algebra
-- Subprograms) for NVIDIA GPUs.
--
-- To use operations from the cuBLAS library, the user must allocate the
-- required matrices and vectors in the GPU memory space, fill them with data,
-- call the desired sequence of cuBLAS functions, then copy the results from the
-- GPU memory space back to the host.
--
-- The <http://hackage.haskell.org/package/cuda cuda> package can be used for
-- writing to and retrieving data from the GPU.
--
-- [/Data layout/]
--
-- Unlike modern BLAS libraries, cuBLAS /only/ provides Fortran-style
-- implementations of the subprograms, using column-major storage and 1-based
-- indexing.
--
-- The @<http://docs.nvidia.com/cuda/cublas/index.html#cublas-lt-t-gt-geam ?geam>@
-- operation can be used to perform matrix transposition.
--
-- [/Example/]
--
-- TBD
--
-- [/Additional information/]
--
-- For more information, see the NVIDIA cuBLAS documentation:
--
-- <http://docs.nvidia.com/cuda/cublas/index.html>
--

module Foreign.CUDA.BLAS (

  -- * Control
  module Foreign.CUDA.BLAS.Context,
  module Foreign.CUDA.BLAS.Stream,
  module Foreign.CUDA.BLAS.Error,

  -- * Operations
  module Foreign.CUDA.BLAS.Level1,
  module Foreign.CUDA.BLAS.Level2,

) where

import Foreign.CUDA.BLAS.Context                hiding ( Handle(Handle, useHandle) )
import Foreign.CUDA.BLAS.Error                  hiding ( resultIfOk, nothingIfOk )
import Foreign.CUDA.BLAS.Stream

import Foreign.CUDA.BLAS.Level1
import Foreign.CUDA.BLAS.Level2

