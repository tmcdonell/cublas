#!/usr/bin/env runhaskell
-- vim: filetype=haskell
--
-- Generate c2hs FFI binding hooks
--
-- Based on: https://github.com/Rufflewind/blas-hs/blob/f8e90b26bc9865618802dce9ccf21fc2b5c032be/tools/generate-ffi
--
-- module Main (main) where

-- import Common

import Data.Char                                                    ( toUpper )
import Data.Functor                                                 ( (<$>) )
import Data.List                                                    ( intercalate )
import Data.Monoid                                                  ( (<>) )
import Text.Printf                                                  ( printf )


-- dotest :: IO ()
-- dotest
--   = putStrLn
--   . mkModule ["ForeignFunctionInterface"] ["Foreign", "CUDA", "BLAS", "Internal", "FFI"] [] [] []
--   . unlines
--   $ map mkFun (funInsts Unsafe)

{--
main :: IO ()
main = forM_ [minBound .. maxBound] $ \ safety ->
  let docs = [ "Stability: Stable"
             , "Foreign function interface to Blas.  These functions use " <>
               (toLower <$> show safety) <> " foreign calls.  Refer to the " <>
               "GHC documentation for more information regarding " <>
               "appropriate use of safe and unsafe foreign calls." ]
      name = [ "Blas", "Primitive", show safety ]
      imps = [ "Prelude (Double, Float, Int, IO, fromIntegral, " <>
               "($), (>>), (>>=), return)"
             , "Data.Complex (Complex)"
             , "Foreign (Ptr, castPtr)"
             , "Foreign.C.Types"
             , "Foreign.Storable.Complex ()"
             , "Blas.Primitive.Types"
             , "BlasCTypes"
             , "FFI" ]
      fis  = funInstsBySafety safety
      exps = [ intercalate ", " (fmap cfName fis) ]
      body = "#include <cblas.h>\n\n" <>
             intercalate "\n\n" (fmap mkFun  fis)
  -- trailing underscore to prevent Cabal from running c2hs on them
  in writeModule "_.chs" exts name docs exps imps body prefix
  where prefix = "../src/"
        exts = [ "ForeignFunctionInterface" ]

funInstsBySafety :: Safety -> [CFun]
funInstsBySafety Safe   = funInsts
funInstsBySafety Unsafe = funInstsUnsafe
--}

main :: IO ()
main =
  let exts    = [ "CPP"
                , "ForeignFunctionInterface"
                ]
      name    = [ "Foreign", "CUDA", "BLAS", "Internal", "FFI" ]
      path    = intercalate "/" name ++ ".chs"
      docs    = [ "-- |"
                , "-- Module      : Foreign.CUDA.BLAS.Internal.FFI"
                , "-- Copyright   : [2014..2017] Trevor L. McDonell"
                , "-- License     : BSD3"
                , "--"
                , "-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>"
                , "-- Stability   : experimental"
                , "-- Portability : non-portable (GHC extensions)"
                , "--"
                ]
      imps    = [ "Data.Complex"
                , "Foreign"
                , "Foreign.Storable.Complex ()"
                , "Foreign.CUDA.Ptr"
                , "Foreign.CUDA.BLAS.Types"
                , "Foreign.CUDA.BLAS.Error"
                ]
      fis     = funInsts Unsafe
      exps    = map cfName fis
      body    = "#include \"cbits/stubs.h\""
              : "{# context lib=\"cublas\" #}"
              : ""
              : "{-# INLINE useDevP #-}"
              : "useDevP :: DevicePtr a -> Ptr b"
              : "useDevP = useDevicePtr . castDevPtr"
              : map mkFun fis
  in
  writeFile path (mkModule exts name docs exps imps body)


mkModule
    :: [String]       -- ^ extensions
    -> [String]       -- ^ module name segments
    -> [String]       -- ^ module documentation paragraphs
    -> [String]       -- ^ exports
    -> [String]       -- ^ imports
    -> [String]       -- ^ module contents
    -> String
mkModule exts name docs exps imps content =
  unlines
    $ "--"
    : "-- This module is auto-generated. Do not edit directly."
    : "--"
    : ""
    : map (printf "{-# LANGUAGE %s #-}") exts
   ++ ""
    : docs
   ++ ""
    : printf "module %s (\n" (intercalate "." name)
    : map (printf "  %s,") exps
   ++ printf "\n) where"
    : ""
    : map (printf "import %s") imps
   ++ ""
    : content


-- | Generates a c2hs hook for the function.
--
mkFun :: CFun -> String
mkFun (CFun safe name params ret doc) =
  intercalate "\n"
    [ if null doc then "" else "-- | " <> doc
    , printf "{-# INLINEABLE %s #-}" name
    , printf "{# fun%s %s%s { %s } -> %s #}" safe' cName hName params' ret'
    ]
  where
    cName   = funMangler name
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
  | TInt
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
    { fName  :: String
    , fTypes :: [Type]
    , _fDoc  :: String
    }

-- | Construct a 'Fun'.
--
fun :: String -> [Type] -> Fun
fun name types = Fun name types ""

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
    { cfSafe    :: Bool
    , cfName    :: String
    , _cfParams :: [Type]
    , _cfRet    :: Type
    , cfDoc     :: String
    }

-- | Construct a 'CFun'.
--
cFun :: String -> [Type] -> Type -> CFun
cFun name params ret = CFun True name params ret ""

-- unreturnable :: Type -> Bool
-- unreturnable t = case t of
--   TComplex TFloat  -> True
--   TComplex TDouble -> True
--   _                -> False

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
  TInt              -> simple "Int"
  TEnum t'          -> enum t'
  TFloat            -> floating "Float"
  TDouble           -> floating "Double"
  -- TComplex t'       -> case t' of
  --   TFloat  -> complex_ "Complex Float"
  --   TDouble -> complex_ "Complex Double"
  --   _       -> error $ "can not marshal type type: " <> show t
  TPtr as t'        -> pointer as $ case t' of
    TInt              -> "Int"
    TFloat            -> "Float"
    TDouble           -> "Double"
    TComplex TFloat   -> "(Complex Float)"
    TComplex TDouble  -> "(Complex Double)"
    _                 -> error $ "can not marshal type: " <> show t
  THandle           -> HType "useHandle" "Handle" ""
  TStatus           -> HType "" "()" "checkStatus*"
  _                 -> error $ "unmarshallable type: " <> show t
  where
    simple s    = HType "" s ""
    enum s      = HType "toCEnum" s "fromCEnum"
    floating s  = HType ("C" <> s) s ("fromC" <> s)
    -- complex_ s  = HType "withVoidPtr*" s ""
    --
    pointer Nothing s       = HType "castPtr"  ("Ptr " <> s) ""
    pointer (Just Host) s   = HType "useHostP" ("HostPtr " <> s) ""
    pointer (Just Device) s = HType "useDevP"  ("DevicePtr " <> s) ""


-- shortcuts

void :: Type
void = TVoid

ptr :: Type -> Type
ptr = TPtr Nothing

dptr :: Type -> Type
dptr = TPtr (Just Device)

hptr :: Type -> Type
hptr = TPtr (Just Host)

int :: Type
int = TInt

float :: Type
float = TFloat

double :: Type
double = TDouble

complex :: Type -> Type
complex = TComplex

index :: Type
index = TInt

transpose :: Type
transpose = TEnum "Operation"

fill :: Type
fill = TEnum "Fill"

diag :: Type
diag = TEnum "Diagonal"

side :: Type
side = TEnum "Side"

funInsts :: Safety -> [CFun]
funInsts safety = mangleFun safety <$> concatFunInstances funs

-- | cuBLAS function signatures. The initial context handle argument is added
-- implicitly.
--
funs :: [FunGroup]
funs =
  -- Level 1
  -- <http://docs.nvidia.com/cuda/cublas/index.html#cublas-level-1-function-reference>
  --
  [ gpA $ \ a   -> fun "i?amax" [ int, dptr a, int, ptr int ]
  , gpA $ \ a   -> fun "i?amin" [ int, dptr a, int, ptr int ]
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

  -- [ gpR $ \ a   -> fun "?dot"   [ int, ptr a, int, ptr a, int, a ]
  -- , gpC $ \ a   -> fun "?dotu"  [ int, ptr a, int, ptr a, int, a ]
  -- , gpC $ \ a   -> fun "?dotc"  [ int, ptr a, int, ptr a, int, a ]
  -- , gpB $ \ a b -> fun "?nrm2"  [ int, ptr b, int, a ]
  -- , gpB $ \ a b -> fun "?asum"  [ int, ptr b, int, a ]
  -- , gpA $ \ a   -> fun "i?amax" [ int, ptr a, int, index ]
  -- , gpA $ \ a   -> fun "?swap"  [ int, ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?copy"  [ int, ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?axpy"  [ int, a, ptr a, int, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?rotg"  [ ptr a, ptr a, ptr a, ptr a, void ]
  -- , gpR $ \ a   -> fun "?rotmg" [ ptr a, ptr a, ptr a, a, ptr a, void ]
  -- , gpR $ \ a   -> fun "?rot"   [ int, ptr a, int, ptr a, int, a, a, void ]
  -- , gpR $ \ a   -> fun "?rotm"  [ int, ptr a, int, ptr a, int, ptr a, void ]
  -- , gpE $ \ a b -> fun "?scal"  [ int, b, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?gemv"  [ order, transpose, int, int, a, ptr a
  --                               , int, ptr a, int, a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?gbmv"  [ order, transpose, int, int, int, int, a
  --                               , ptr a, int, ptr a, int, a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?trmv"  [ order, uplo, transpose, diag, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?tbmv"  [ order, uplo, transpose, diag, int, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?tpmv"  [ order, uplo, transpose, diag, int
  --                               , ptr a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?trsv"  [ order, uplo, transpose, diag, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?tbsv"  [ order, uplo, transpose, diag, int, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?tpsv"  [ order, uplo, transpose, diag, int
  --                               , ptr a, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?symv"  [ order, uplo, int, a, ptr a, int, ptr a, int
  --                               , a, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?sbmv"  [ order, uplo, int, int, a, ptr a, int, ptr a
  --                               , int, a, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?spmv"  [ order, uplo, int, a, ptr a, ptr a, int, a
  --                               , ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?ger"   [ order, int, int, a, ptr a, int, ptr a
  --                               , int, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?syr"   [ order, uplo, int, a, ptr a, int, ptr a
  --                               , int, void ]
  -- , gpR $ \ a   -> fun "?syr2"  [ order, uplo, int, a, ptr a, int, ptr a
  --                               , int, ptr a, int, void ]
  -- , gpR $ \ a   -> fun "?spr"   [ order, uplo, int, a, ptr a
  --                               , int, ptr a, void ]
  -- , gpR $ \ a   -> fun "?spr2"  [ order, uplo, int, a, ptr a, int, ptr a
  --                               , int, ptr a, void ]
  -- , gpC $ \ a   -> fun "?hemv"  [ order, uplo, int, a, ptr a, int, ptr a
  --                               , int, a, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?hbmv"  [ order, uplo, int, int, a, ptr a
  --                               , int, ptr a, int, a, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?hpmv"  [ order, uplo, int, a, ptr a, ptr a
  --                               , int, a, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?geru"  [ order, int, int, a, ptr a, int
  --                               , ptr a , int, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?gerc"  [ order, int, int, a, ptr a, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpQ $ \ a   -> fun "?her"   [ order, uplo, int, a, ptr (complex a), int
  --                               , ptr (complex a), int, void ]
  -- , gpQ $ \ a   -> fun "?hpr"   [ order, uplo, int, a, ptr (complex a), int
  --                               , ptr (complex a), void ]
  -- , gpC $ \ a   -> fun "?her2"  [ order, uplo, int, a, ptr a, int
  --                               , ptr a, int, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?hpr2"  [ order, uplo, int, a, ptr a, int
  --                               , ptr a, int, ptr a, void ]
  -- , gpA $ \ a   -> fun "?gemm"  [ order, transpose, transpose, int, int, int, a
  --                               , ptr a, int, ptr a, int, a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?symm"  [ order, side, uplo, int, int, a, ptr a, int
  --                               , ptr a, int, a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?syrk"  [ order, uplo, transpose, int, int, a, ptr a
  --                               , int, a, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?syr2k" [ order, uplo, transpose, int, int, a, ptr a
  --                               , int, ptr a, int, a, ptr a, int, void ]
  -- , gpC $ \ a   -> fun "?hemm"  [ order, side, uplo, int, int, a, ptr a, int
  --                               , ptr a, int, a, ptr a, int, void ]
  -- , gpQ $ \ a   -> fun "?herk"  [ order, uplo, transpose, int, int, a
  --                               , ptr (complex a), int, a
  --                               , ptr (complex a), int, void ]
  -- , gpQ $ \ a   -> fun "?her2k" [ order, uplo, transpose, int, int, complex a
  --                               , ptr (complex a), int, ptr (complex a)
  --                               , int, a, ptr (complex a), int, void ]
  -- , gpA $ \ a   -> fun "?trmm"  [ order, side, uplo, transpose, diag, int, int
  --                               , a, ptr a, int, ptr a, int, void ]
  -- , gpA $ \ a   -> fun "?trsm"  [ order, side, uplo, transpose, diag, int, int
  --                               , a, ptr a, int, ptr a, int, void ]
  -- ]

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

funMangler :: String -> String
funMangler []     = error "funMangler: empty input"
funMangler (x:xs) = printf "cublas%c%s_v2" (toUpper x) xs

mangleFun :: Safety -> Fun -> CFun
mangleFun safety (Fun name params doc) =
  CFun (safety==Safe) name (THandle : params) TStatus doc

