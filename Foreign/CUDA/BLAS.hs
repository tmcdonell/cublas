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
-- At a short example, we show how to compute the following matrix-matrix
-- product with 'dgemm':
--
-- \[
-- \left(\begin{matrix} 1 & 2 \\ 3 & 4 \\ 5 & 6 \\ \end{matrix}\right) \cdot
-- \left(\begin{matrix} 1 & 2 & 3 \\ 4 & 5 & 6 \\ \end{matrix}\right)  =
-- \left(\begin{matrix} 9 & 12 & 15 \\ 19 & 26 & 33 \\ 29 & 40 & 51 \\ \end{matrix}\right)
-- \]
--
-- I assume you know how to initialise the CUDA
-- environment, as described in the "Foreign.CUDA.Driver" module:
--
-- >>> import Foreign.CUDA.Driver as CUDA
-- >>> import Foreign.CUDA.BLAS as BLAS
-- >>> CUDA.initialise []
-- >>> dev <- CUDA.device 0
-- >>> ctx <- CUDA.create dev []
--
-- Just as we must create a CUDA execution context with
-- 'Foreign.CUDA.Driver.create' before interacting with the GPU, we must create
-- a BLAS context handle before executing any cuBLAS library operations, which
-- will be associated with the current device context:
--
-- >>> hdl <- BLAS.create
--
-- Now, let us generate the matrix data on the GPU. (For simplicity in this
-- example we will just marshal the data via lists, but in a real application
-- with a large amount of data we should of course use some kind of unboxed
-- array):
--
-- >>> let rowsA = 3; colsA = 2; sizeA = rowsA * colsA
-- >>> let rowsB = 2; colsB = 3; sizeB = rowsB * colsB
-- >>> let sizeC = rowsA * colsB
-- >>> matA <- CUDA.newListArray (take sizeA [1..])
-- >>> matB <- CUDA.newListArray (take sizeB [1..])
-- >>> matC <- CUDA.mallocArray sizeC
--
-- Note in the above that we store data in row-major order, as is the convention
-- in C. However, the cuBLAS library assumes a column-major representation, as
-- is the style of Fortran. However, we can make use of the following
-- equivalency:
--
-- \[
-- B^T \cdot A^T = (A \cdot B)^T
-- \]
--
-- and, since the transposed matrix in column-major representation is equivalent
-- to our matrix in row-major representation, we can avoid any actual data
-- manipulation to get things into a form suitable for cuBLAS (phew!).
--
-- The final thing to take care of are the scaling parameters to the 'dgemm'
-- operation, \(\alpha\) and \(\beta\). By default, it is assumed that these
-- values reside in host memory, but this setting can be changed with
-- 'setPointerMode'; When set to 'Device' mode, the function
-- 'Foreign.CUDA.Ptr.withDevicePtr' can be used to treat the device memory
-- pointer as a plain pointer to pass to the function.
--
-- Now, we are ready to piece it all together:
--
-- >>> import Foreign.Marshal
-- >>> with 1.0 $ \alpha ->
-- >>> with 0.0 $ \beta ->
-- >>>   dgemm hdl N N colsB rowsA colsA alpha matB colsB matA colsA beta matC colsB
--
-- And retrieve the result:
--
-- >>> print =<< CUDA.peekListArray sizeC matC
-- [9.0,12.0,15.0,19.0,26.0,33.0,29.0,40.0,51.0]
--
-- Finally, we should 'Foreign.CUDA.Driver.free' the device memory we allocated,
-- and release the BLAS context handle:
--
-- >>> BLAS.destroy hdl
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
  module Foreign.CUDA.BLAS.Level3,

) where

import Foreign.CUDA.BLAS.Context                hiding ( Handle(Handle, useHandle) )
import Foreign.CUDA.BLAS.Error                  hiding ( resultIfOk, nothingIfOk )
import Foreign.CUDA.BLAS.Stream

import Foreign.CUDA.BLAS.Level1
import Foreign.CUDA.BLAS.Level2
import Foreign.CUDA.BLAS.Level3

