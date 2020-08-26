# Revision history for cublas

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)


## [0.6.0.0] - 2020-08-26
### Added
  * Added support for Cabal-3

### Fixed
  * Fixed linking with CUDA-10.x

## [0.5.0.0] - 2018-10-02
### Added
  * `gemm[Strided]BatchedEx` from CUDA-9.1


## [0.4.0.1] - 2018-03-12
### Fixed
  * Build fix for Cabal-2.2 (ghc-8.4)

## [0.4.0.0] - 2017-11-15
### Added
  * `getMathMode`
  * `setMathMode`

### Fixed
  * Build fix for CUDA-9

## 0.3.0.0 - 2017-08-24

* First version; replaces [bmsherman/cublas](https://github.com/bmsherman/cublas). Released on an unsuspecting world.


[0.6.0.0]:          https://github.com/tmcdonell/cublas/compare/release/v0.5.0.0...v0.6.0.0
[0.5.0.0]:          https://github.com/tmcdonell/cublas/compare/release/0.4.0.1...v0.5.0.0
[0.4.0.1]:          https://github.com/tmcdonell/cublas/compare/release/0.4.0.0...0.4.0.1
[0.4.0.0]:          https://github.com/tmcdonell/cublas/compare/release/0.3.0.0...0.4.0.0

