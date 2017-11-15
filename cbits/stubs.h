/*
 * Extra bits for CUBLAS bindings
 */

#ifndef C_STUBS_H
#define C_STUBS_H

#ifdef __MINGW32__
#include <host_defines.h>
#undef CUDARTAPI
#define CUDARTAPI __stdcall
#endif

#include <cuda.h>
#include <cublas_v2.h>

/*
 * We need to redeclare these functions for CUDA-9, as they are now hidden
 * behind a #if defined(__cplusplus) guard.
 */
#if CUDA_VERSION >= 9000
typedef struct __align__(2) {
   unsigned short x;
} __half;

CUBLASAPI cublasStatus_t CUBLASWINAPI cublasHgemm    (cublasHandle_t handle,
                                                      cublasOperation_t transa,
                                                      cublasOperation_t transb,
                                                      int m,
                                                      int n,
                                                      int k,
                                                      const __half *alpha, /* host or device pointer */
                                                      const __half *A,
                                                      int lda,
                                                      const __half *B,
                                                      int ldb,
                                                      const __half *beta, /* host or device pointer */
                                                      __half *C,
                                                      int ldc);

CUBLASAPI cublasStatus_t CUBLASWINAPI cublasHgemmBatched (cublasHandle_t handle,
                                                          cublasOperation_t transa,
                                                          cublasOperation_t transb,
                                                          int m,
                                                          int n,
                                                          int k,
                                                          const __half *alpha,  /* host or device pointer */
                                                          const __half *Aarray[],
                                                          int lda,
                                                          const __half *Barray[],
                                                          int ldb,
                                                          const __half *beta,   /* host or device pointer */
                                                          __half *Carray[],
                                                          int ldc,
                                                          int batchCount);

CUBLASAPI cublasStatus_t CUBLASWINAPI cublasHgemmStridedBatched (cublasHandle_t handle,
                                                                 cublasOperation_t transa,
                                                                 cublasOperation_t transb,
                                                                 int m,
                                                                 int n,
                                                                 int k,
                                                                 const __half *alpha,  /* host or device pointer */
                                                                 const __half *A,
                                                                 int lda,
                                                                 long long int strideA,   /* purposely signed */
                                                                 const __half *B,
                                                                 int ldb,
                                                                 long long int strideB,
                                                                 const __half *beta,   /* host or device pointer */
                                                                 __half *C,
                                                                 int ldc,
                                                                 long long int strideC,
                                                                 int batchCount);
#endif /* CUDA_VERSION */
#endif /* C_STUBS_H */

