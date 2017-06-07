#!/usr/bin/env runhaskell
-- vim: filetype=haskell
--
-- Generate c2hs FFI binding hooks
--
-- Based on: https://github.com/Rufflewind/blas-hs/blob/f8e90b26bc9865618802dce9ccf21fc2b5c032be/tools/generate-ffi
--
module Main (main) where

import Data.Char                                                    ( toUpper )
import Data.Functor                                                 ( (<$>) )
import Data.List                                                    ( intercalate )
import Data.Monoid                                                  ( (<>) )
import Text.Printf                                                  ( printf )


main :: IO ()
main = do
  let
      docs :: Int -> [String]
      docs l  = [ printf "For more information see the cuBLAS Level-%d function reference:" l
                , ""
                , printf "<http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-%d-function-reference>" l
                , ""
                ]
      l1exps  = []
      l2exps  = [ "Operation(..)"
                , "Fill(..)"
                , "Diagonal(..)"
                ]
      l3exps  = l2exps ++
                [ "Side(..)"
                , "Type(..)"
                , "GemmAlgorithm(..)"
                ]
  --
  mkC2HS "Level1" (docs 1) l1exps [(Nothing,   funsL1)]
  mkC2HS "Level2" (docs 2) l2exps [(Nothing,   funsL2)]
  mkC2HS "Level3" (docs 3) l3exps [(Nothing,   funsL3)
                                  ,(Just 8000, funsL3_c8)
                                  ]


mkC2HS :: String -> [String] -> [String] -> [(Maybe Int, [FunGroup])] -> IO ()
mkC2HS mdl docs exps funs =
  let exts    = [ "CPP"
                , "ForeignFunctionInterface"
                ]
      name    = [ "Foreign", "CUDA", "BLAS", mdl ]
      path    = intercalate "/" name ++ ".chs"
      imps    = [ "Data.Complex"
                , "Numeric.Half"
                , "Foreign"
                , "Foreign.C"
                , "Foreign.Storable.Complex ()"
                , "Foreign.CUDA.Ptr"
                , "Foreign.CUDA.BLAS.Internal.C2HS"
                , "Foreign.CUDA.BLAS.Internal.Types"
                ]
      body    = "{-# INLINE useDevP #-}"
              : "useDevP :: DevicePtr a -> Ptr b"
              : "useDevP = useDevicePtr . castDevPtr"
              : ""
              : "{-# INLINE useHostP #-}"
              : "useHostP :: HostPtr a -> Ptr b"
              : "useHostP = useHostPtr . castHostPtr"
              : ""
              : content

      wrap f (Nothing, fg) = f fg
      wrap f (Just v,  fg) = printf "#if CUDA_VERSION >= %d" v
                           : f fg
                          ++ ["#endif"]

      fis     = map (\(r,f) -> (r, funInsts Unsafe f)) funs
      exps'   = exps ++ concatMap (wrap (map cfName)) fis
      content = concatMap (wrap (map mkFun)) fis
  in
  writeFile path $ mkModule exts name docs exps' imps body


mkModule
    :: [String]       -- ^ extensions
    -> [String]       -- ^ module name segments
    -> [String]       -- ^ module documentation
    -> [String]       -- ^ exports
    -> [String]       -- ^ imports
    -> [String]       -- ^ module contents
    -> String
mkModule exts name docs exps imps body =
  unlines
    $ "--"
    : "-- This module is auto-generated. Do not edit directly."
    : "--"
    : ""
    : "#include \"cbits/stubs.h\""
    : "{# context lib=\"cublas\" #}"
    : ""
    : map (printf "{-# LANGUAGE %s #-}") exts
   ++ "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    : "{-# OPTIONS_GHC -fno-warn-unused-binds #-}"
    : "-- |"
    :("-- Module      : " ++ intercalate "." name)
    : "-- Copyright   : [2017] Trevor L. McDonell"
    : "-- License     : BSD3"
    : "--"
    : "-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>"
    : "-- Stability   : experimental"
    : "-- Portability : non-portable (GHC extensions)"
    : "--"
    : map (\x -> if null x then "--" else "-- " ++ x) docs
   ++ ""
    : printf "module %s (\n" (intercalate "." name)
    : map (\x -> if head x == '#' then x else printf "  %s," x) exps
   ++ printf "\n) where"
    : ""
    : map (printf "import %s") imps
   ++ ""
    : body


-- | Generates a c2hs hook for the function.
--
mkFun :: CFun -> String
mkFun (CFun safe name suffix params ret doc) =
  intercalate "\n"
    [ if null doc then "" else "-- | " <> doc
    , printf "{-# INLINEABLE %s #-}" name
    , printf "{# fun%s %s%s { %s } -> %s #}" safe' cName hName params' ret'
    ]
  where
    cName   = funMangler name suffix
    hName   = if name == cName then "" else " as " <> name
    safe'   = if safe then "" else " unsafe"
    params' = intercalate ", " $ fmap (mkParamType . convType) params
    ret'    = mkRetType $ convType ret

data Safety
  = Safe
  | Unsafe
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Represents a C type.
--
data Type
  = THandle
  | TStatus
  | TVoid
  | TPtr (Maybe AddrSpace) Type
  | TInt (Maybe Int)
  | THalf -- 16-bit floating-point type
  | TFloat
  | TDouble
  | TComplex Type
  | TEnum String
  | TDummy Int                    -- ^ Used for extracting the bound variables
  deriving (Eq, Show)

data AddrSpace
  = Host | Device
  deriving (Eq, Show)

realTypes :: [Type]
realTypes = [ float, double ]

complexTypes :: [Type]
complexTypes = complex <$> realTypes

floatingTypes :: [Type]
floatingTypes = realTypes <> complexTypes

floatingTypesB :: [(Type, Type)]
floatingTypesB = do
  t <- floatingTypes
  return $ case t of
    TComplex t' -> (t', t)
    _           -> (t,  t)

floatingTypesE :: [(Type, Type)]
floatingTypesE = do
  t <- floatingTypes
  case t of
    TComplex t' -> [(t, t), (t, t')]
    _           -> [(t, t)]

-- | Represents a C function.
--
data Fun
  = Fun
    { fName     :: String
    , _fSuffix  :: String
    , fTypes    :: [Type]
    , _fDoc     :: String
    }

-- | Construct a 'Fun'.
--
fun :: String -> [Type] -> Fun
fun name types = Fun name "_v2" types ""

ext :: String -> [Type] -> Fun
ext name types = Fun name "" types ""

-- | Represents a marshallable C type for c2hs.
--
data HType = HType
             String                     -- in marshaller
             String                     -- type
             String                     -- out marshaller
             deriving Show

mkParamType :: HType -> String
mkParamType (HType m s _) =
  if null m then s' else m <> " " <> s'
  where s' = "`" <> s <> "'"

mkRetType :: HType -> String
mkRetType (HType _ s m) =
  if null m then s' else s' <> " " <> m
  where s' = "`" <> s <> "'"

-- | Represents a C function hook for c2hs.
--
data CFun
  = CFun
    { _cfSafe   :: Bool
    , cfName    :: String
    , _cfSuffix :: String
    , _cfParams :: [Type]
    , _cfRet    :: Type
    , _cfDoc    :: String
    }

-- | Construct a 'CFun'.
--
-- cFun :: String -> String -> [Type] -> Type -> CFun
-- cFun name suffix params ret = CFun True name suffix params ret ""

substitute :: String -> String -> String
substitute s y = case y of
  []     -> []
  x : xs ->
    let xs' = substitute s xs in
    case x of
      '?' -> s <> xs'
      _   -> x : xs'

typeAbbrev :: Type -> String
typeAbbrev t = case t of
  THalf            -> "h"
  TFloat           -> "s"
  TDouble          -> "d"
  TComplex TFloat  -> "c"
  TComplex TDouble -> "z"
  _                -> error ("no valid abbreviation for: " <> show t)

decorate :: [Type] -> String -> String
decorate [a]                = substitute $ typeAbbrev a
decorate [a, b] | a == b    = substitute $ typeAbbrev a
                | otherwise = substitute $ typeAbbrev a <> typeAbbrev b
decorate _                  = error "decorate: bad args"

-- NOTE: Here we assume that both the C and Haskell types have identical
-- representations; this isn't in the specs but in practice the Storable
-- instances are identical so it should work fine
--
convType :: Type -> HType
convType t = case t of
  TVoid             -> simple "()"
  TInt ms           -> simple (maybe "Int" (printf "Int%d") ms)
  TEnum t'          -> enum t'
  THalf             -> floating "Half"
  TFloat            -> floating "Float"
  TDouble           -> floating "Double"
  TComplex TFloat   -> simple "(Complex Float)"
  TComplex TDouble  -> simple "(Complex Double)"
  TPtr as t'        -> pointer as
                     $ case convType t' of
                         HType _ s _ -> case t' of
                                          TPtr{} -> printf "(%s)" s
                                          _      -> s
  THandle           -> HType "useHandle" "Handle" ""
  TStatus           -> HType "" "()" "checkStatus*"
  _                 -> error $ "unmarshallable type: " <> show t
  where
    simple s    = HType "" s ""
    enum s      = HType "cFromEnum" s "cToEnum"
    floating s  = HType ("C" <> s) s ("fromC" <> s)
    --
    pointer Nothing s       = HType "castPtr"  ("Ptr " <> s) ""
    pointer (Just Host) s   = HType "useHostP" ("HostPtr " <> s) ""
    pointer (Just Device) s = HType "useDevP"  ("DevicePtr " <> s) ""


-- shortcuts

ptr :: Type -> Type
ptr = TPtr Nothing

dptr :: Type -> Type
dptr = TPtr (Just Device)

hptr :: Type -> Type
hptr = TPtr (Just Host)

void :: Type
void = TVoid

int :: Type
int = TInt Nothing

int32 :: Type
int32 = TInt (Just 32)

int64 :: Type
int64 = TInt (Just 64)

half :: Type
half = THalf

float :: Type
float = TFloat

double :: Type
double = TDouble

complex :: Type -> Type
complex = TComplex

transpose :: Type
transpose = TEnum "Operation"

uplo :: Type
uplo = TEnum "Fill"

diag :: Type
diag = TEnum "Diagonal"

side :: Type
side = TEnum "Side"

dtype :: Type
dtype = TEnum "Type"

funInsts :: Safety -> [FunGroup] -> [CFun]
funInsts safety funs = mangleFun safety <$> concatFunInstances funs

-- | cuBLAS function signatures. The initial context handle argument is added
-- implicitly.
--
-- Level 1 (vector-vector) operations.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-1-function-reference>
--
funsL1 :: [FunGroup]
funsL1 =
  [ gpA $ \ a   -> fun "i?amax" [ int, dptr a, int, ptr int32 ]
  , gpA $ \ a   -> fun "i?amin" [ int, dptr a, int, ptr int32 ]
  , gpB $ \ a b -> fun "?asum"  [ int, dptr b, int, ptr a ]
  , gpA $ \ a   -> fun "?axpy"  [ int, ptr a, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?copy"  [ int, dptr a, int, dptr a, int ]
  , gpR $ \ a   -> fun "?dot"   [ int, dptr a, int, dptr a, int, ptr a ]
  , gpC $ \ a   -> fun "?dotu"  [ int, dptr a, int, dptr a, int, ptr a ]
  , gpC $ \ a   -> fun "?dotc"  [ int, dptr a, int, dptr a, int, ptr a ]
  , gpB $ \ a b -> fun "?nrm2"  [ int, dptr b, int, ptr a ]
  , gpE $ \ a b -> fun "?rot"   [ int, dptr a, int, dptr a, int, ptr b, ptr b ]
  , gpA $ \ a   -> fun "?rotg"  [ ptr a, ptr a, ptr a, ptr a ]
  , gpR $ \ a   -> fun "?rotm"  [ int, dptr a, int, dptr a, int, ptr a ]
  , gpR $ \ a   -> fun "?rotmg" [ ptr a, ptr a, ptr a, ptr a, ptr a ]
  , gpE $ \ a b -> fun "?scal"  [ int, ptr b, dptr a, int ]
  , gpA $ \ a   -> fun "?swap"  [ int, dptr a, int, dptr a, int ]
  ]

-- Level 2 (matrix-vector) operations.
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-2-function-reference>
--
funsL2 :: [FunGroup]
funsL2 =
  [ gpA $ \ a   -> fun "?gbmv"  [ transpose, int, int, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?gemv"  [ transpose, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpR $ \ a   -> fun "?ger"   [ int, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpC $ \ a   -> fun "?gerc"  [ int, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpC $ \ a   -> fun "?geru"  [ int, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpR $ \ a   -> fun "?sbmv"  [ uplo, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpR $ \ a   -> fun "?spmv"  [ uplo, int, ptr a, dptr a, dptr a, int, ptr a, dptr a, int ]
  , gpR $ \ a   -> fun "?spr"   [ uplo, int, ptr a, dptr a, int, dptr a ]
  , gpR $ \ a   -> fun "?spr2"  [ uplo, int, ptr a, dptr a, int, dptr a, int, dptr a ]
  , gpA $ \ a   -> fun "?symv"  [ uplo, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?syr"   [ uplo, int, ptr a, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?syr2"  [ uplo, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?tbmv"  [ uplo, transpose, diag, int, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?tbsv"  [ uplo, transpose, diag, int, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?tpmv"  [ uplo, transpose, diag, int, dptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?tpsv"  [ uplo, transpose, diag, int, dptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?trmv"  [ uplo, transpose, diag, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?trsv"  [ uplo, transpose, diag, int, dptr a, int, dptr a, int ]
  , gpC $ \ a   -> fun "?hemv"  [ uplo, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpC $ \ a   -> fun "?hbmv"  [ uplo, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpC $ \ a   -> fun "?hpmv"  [ uplo, int, ptr a, dptr a, dptr a, int, ptr a, dptr a, int ]
  , gpQ $ \ a   -> fun "?her"   [ uplo, int, ptr a, dptr (complex a), int, dptr (complex a), int ]
  , gpC $ \ a   -> fun "?her2"  [ uplo, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpQ $ \ a   -> fun "?hpr"   [ uplo, int, ptr a, dptr (complex a), int, dptr (complex a) ]
  , gpC $ \ a   -> fun "?hpr2"  [ uplo, int, ptr a, dptr a, int, dptr a, int, dptr a ]
  ]

-- Level 3 (matrix-vector) operations (and extensions)
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-3-function-reference>
--
-- <http://docs.nvidia.com/cuda/cublas/index.html#blas-like-extension>
--
funsL3 :: [FunGroup]
funsL3 =
  [ gpA $ \ a   -> fun "?gemm"          [ transpose, transpose, int, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gp  $          ext "hgemm"          [ transpose, transpose, int, int, int, ptr half, dptr half, int, dptr half, int, ptr half, dptr half, int ]
  , gpA $ \ a   -> ext "?gemmBatched"   [ transpose, transpose, int, int, int, ptr a, dptr (dptr a), int, dptr (dptr a), int, ptr a, dptr (dptr a), int, int ]
  , gpA $ \ a   -> fun "?symm"          [ side, uplo, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?syrk"          [ uplo, transpose, int, int, ptr a, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?syr2k"         [ uplo, transpose, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> ext "?syrkx"         [ uplo, transpose, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?trmm"          [ side, uplo, transpose, diag, int, int, ptr a, dptr a, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?trsm"          [ side, uplo, transpose, diag, int, int, ptr a, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> ext "?trsmBatched"   [ side, uplo, transpose, diag, int, int, ptr a, dptr (dptr a), int, dptr (dptr a), int, int ]
  , gpC $ \ a   -> fun "?hemm"          [ side, uplo, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpQ $ \ a   -> fun "?herk"          [ uplo, transpose, int, int, ptr a, dptr (complex a), int, ptr a, dptr (complex a), int ]
  , gpQ $ \ a   -> fun "?her2k"         [ uplo, transpose, int, int, ptr (complex a), dptr (complex a), int, dptr (complex a), int, ptr a, dptr (complex a), int ]
  , gpQ $ \ a   -> ext "?herkx"         [ uplo, transpose, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]

  -- BLAS-like extensions
  , gpA $ \ a   -> ext "?geam"          [ transpose, transpose, int, int, ptr a, dptr a, int, ptr a, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> ext "?dgmm"          [ side, int, int, dptr a, int, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> ext "?getrfBatched"  [ int, dptr (dptr a), int, ptr int32, ptr int32, int ]
  , gpA $ \ a   -> ext "?getrsBatched"  [ transpose, int, int, dptr (dptr a), int, dptr int32, dptr (dptr a), int, hptr int32, int ]
  , gpA $ \ a   -> ext "?getriBatched"  [ int, dptr (dptr a), int, dptr int32, dptr (dptr a), int, dptr int32, int ]
  , gpA $ \ a   -> ext "?matinvBatched" [ int, dptr (dptr a), int, dptr (dptr a), int, dptr int32, int ]
  , gpA $ \ a   -> ext "?geqrfBatched"  [ int, int, dptr (dptr a), int, dptr (dptr a), hptr int32, int ]
  , gpA $ \ a   -> ext "?gelsBatched"   [ transpose, int, int, int, dptr (dptr a), int, dptr (dptr a), int, hptr int32, dptr int32, int ]
  , gpA $ \ a   -> ext "?tpttr"         [ uplo, int, dptr a, dptr a, int ]
  , gpA $ \ a   -> ext "?trttp"         [ uplo, int, dptr a, int, dptr a ]
  , gp  $          ext "sgemmEx"        [ transpose, transpose, int, int, int, ptr float, dptr void, dtype, int, dptr void, dtype, int, ptr float, dptr void, dtype, int ]
  ]

-- Level 3 operations introduced in CUDA-8
--
funsL3_c8 :: [FunGroup]
funsL3_c8 =
  [ gpC $ \ a   -> ext "?gemm3m"    [ transpose, transpose, int, int, int, ptr a, dptr a, int, dptr a, int, ptr a, dptr a, int ]
  , gpH $ \ a   -> ext "?gemmStridedBatched"
                                    [ transpose, transpose, int, int, int, ptr a, dptr a, int, int64, dptr a, int, int64, ptr a, dptr a, int, int64, int ]
  , gp  $          ext "cgemm3mStridedBatched"
                                    [ transpose, transpose, int, int, int, ptr (complex float), dptr (complex float), int, int64, dptr (complex float), int, int64, ptr (complex float), dptr (complex float), int, int64, int ]
  , gp  $          ext "cgemmEx"    [ transpose, transpose, int, int, int, ptr (complex float), dptr void, dtype, int, dptr void, dtype, int, ptr (complex float), dptr void, dtype, int ]
  , gp  $          ext "gemmEx"     [ transpose, transpose, int, int, int, ptr void, dptr void, dtype, int, dptr void, dtype, int, ptr void, dptr void, dtype, int, dtype, TEnum "GemmAlgorithm" ]
  , gp  $          ext "csyrkEx"    [ uplo, transpose, int, int, ptr float, dptr void, dtype, int, ptr float, dptr (complex float), dtype, int ]
  , gp  $          ext "csyrk3mEx"  [ uplo, transpose, int, int, ptr float, dptr void, dtype, int, ptr float, dptr (complex float), dtype, int ]
  , gp  $          ext "cherkEx"    [ uplo, transpose, int, int, ptr float, dptr void, dtype, int, ptr float, dptr (complex float), dtype, int ]
  , gp  $          ext "cherk3mEx"  [ uplo, transpose, int, int, ptr float, dptr void, dtype, int, ptr float, dptr (complex float), dtype, int ]
  , gp  $          ext "nrm2Ex"     [ int, dptr void, dtype, int, ptr void, dtype, dtype ]
  , gp  $          ext "axpyEx"     [ int, ptr void, dtype, dptr void, dtype, int, dptr void, dtype, int, dtype ]
  , gp  $          ext "dotEx"      [ int, dptr void, dtype, int, dptr void, dtype, int, ptr void, dtype, dtype ]
  , gp  $          ext "dotcEx"     [ int, dptr void, dtype, int, dptr void, dtype, int, ptr void, dtype, dtype ]
  , gp  $          ext "scalEx"     [ int, ptr void, dtype, dptr void, dtype, int, dtype ]
  ]


data FunGroup
  = FunGroup
    { _gpName :: String
    , _gpType :: [Type]
    , gpInsts :: [FunInstance]
    }

gp :: Fun -> FunGroup
gp f = FunGroup (fName f) (fTypes f) [FunInstance [] f]

-- | Function group over @s d c z@.
gpA :: (Type -> Fun) -> FunGroup
gpA = makeFunGroup1 decorate floatingTypes

-- | Function group over @s d c z h@
gpH :: (Type -> Fun) -> FunGroup
gpH = makeFunGroup1 decorate (floatingTypes <> return half)

-- | Function group over @s d@.
gpR :: (Type -> Fun) -> FunGroup
gpR = makeFunGroup1 decorate realTypes

-- | Function group over @s d@ but relabel them as @c z@.
gpQ :: (Type -> Fun) -> FunGroup
gpQ = makeFunGroup1 (decorate . (complex <$>)) realTypes

-- | Function group over @c z@.
gpC :: (Type -> Fun) -> FunGroup
gpC = makeFunGroup1 decorate complexTypes

-- | Function group over @ss dd sc dz@.
gpB :: (Type -> Type -> Fun) -> FunGroup
gpB = makeFunGroup2 decorate floatingTypesB

-- | Function group over @ss dd cc zz cs zd@.
gpE :: (Type -> Type -> Fun) -> FunGroup
gpE = makeFunGroup2 decorate floatingTypesE

makeFunGroup1 :: ([Type] -> String -> String)
              -> [Type]
              -> (Type -> Fun)
              -> FunGroup
makeFunGroup1 d ts ff = makeFunGroup 1 d ts' ff'
  where ts'      = [ [a] | a <- ts ]
        ff' args = ff a   where [a]    = args

makeFunGroup2 :: ([Type] -> String -> String)
              -> [(Type, Type)]
              -> (Type -> Type -> Fun)
              -> FunGroup
makeFunGroup2 d ts ff = makeFunGroup 2 d ts' ff'
  where ts'      = [ [a, b] | (a, b) <- ts ]
        ff' args = ff a b where [a, b] = args

makeFunGroup :: Int
             -> ([Type] -> String -> String)
             -> [[Type]]
             -> ([Type] -> Fun)
             -> FunGroup
makeFunGroup n decorator ts ff =
  let f = ff (take n (TDummy <$> [0 ..])) in
  FunGroup (substitute "" $ fName f) (fTypes f) $ do
    t <- ts
    let f' = ff t
    return $ FunInstance t (f' { fName = decorator t $ fName f'})

data FunInstance
  = FunInstance
    { _fiArgs :: [Type]
    , fiFun   :: Fun
    }

concatFunInstances :: [FunGroup] -> [Fun]
concatFunInstances = (>>= (>>= return . fiFun) . gpInsts)

funMangler :: String -> String -> String
funMangler []     _   = error "funMangler: empty input"
funMangler (x:xs) suf = printf "cublas%c%s%s" (toUpper x) xs suf

mangleFun :: Safety -> Fun -> CFun
mangleFun safety (Fun name suffix params doc) =
  CFun (safety==Safe) name suffix (THandle : params) TStatus doc

