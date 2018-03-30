module Fluid.Types where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Type.Proxy (Proxy(..))
import Data.Tuple
import Data.Tuple.Nested

type Pull =
  { protocol :: String
  , host :: String
  , path :: String
  , port :: Int
  }

pullAddress :: Pull -> String
pullAddress {protocol,host,path,port} = protocol <> "://" <> host <> ":" <> show port <> path

type Version =
  { major :: Int
  , minor :: Int
  }

type TypeName = String

type EnumeralName = String

type MemberName = String

data RuntimeError
  = RuntimeError'UnparsableFormat
  | RuntimeError'UnrecognizedCall TypeName
  | RuntimeError'VariableLimit
  | RuntimeError'LangServiceCallLimit Int
  | RuntimeError'LangLambdaLimit Int
  | RuntimeError'LangExprLimit Int
  | RuntimeError'UnknownVariable String
  | RuntimeError'IncompatibleType
  | RuntimeError'MissingMatchCase
  | RuntimeError'TooFewArguments
  | RuntimeError'TooManyArguments
  | RuntimeError'NoApiVersion
  | RuntimeError'NoFluidVersion
  | RuntimeError'ApiMajorVersionTooLow
  | RuntimeError'ApiMajorVersionTooHigh
  | RuntimeError'ApiMinorVersionTooHigh
  | RuntimeError'FluidMajorVersionTooLow
  | RuntimeError'FluidMajorVersionTooHigh
  | RuntimeError'FluidMinorVersionTooHigh
  | RuntimeError'UnparsableMeta
  | RuntimeError'UnparsableQuery
  | RuntimeError'NoImplementation
  | RuntimeError'NotMember

type Limits =
  { variables :: Maybe Int
  , serviceCalls :: Maybe Int
  , lambdas :: Maybe Int
  , expressions :: Maybe Int
  }

type Hooks m meta meta' =
  { metaMiddleware :: meta -> m meta'
  , sandboxLimits :: meta' -> m Limits
  }

type Symbol = String

data Type = Type
  { n :: TypeName
  , p :: Array Type
  , o :: Maybe Type
  }

data Const
  = Const'Null
  | Const'Bool Boolean
  | Const'String String
  | Const'Number Number

data Infer
  = Infer'Null
  | Infer'Number Number

class HasType a where
  getType :: Proxy a -> Type

stringToType :: String -> Type
stringToType s = Type { n: s, p: [], o: Nothing }

instance hasTypeUnit :: HasType Unit where
  getType _ = stringToType "Unit"

instance hasTypeBoolean :: HasType Boolean where
  getType _ = stringToType "Bool"

instance hasTypeString :: HasType String where
  getType _ = stringToType "String"

instance hasTypeInt :: HasType Int where
  getType _ = stringToType "Int"

instance hasTypeNumber :: HasType Number where
  getType _ = stringToType "Float"

instance hasTypeMaybe :: HasType a => HasType (Maybe a) where
  getType x = Type { n: "Option", p: [getType (p x)], o: Nothing }
    where
      p :: Proxy (Maybe a) -> Proxy a
      p _ = Proxy

instance hasTypeArray :: HasType a => HasType (Array a) where
  getType x = Type { n: "List", p: [getType (p x)], o: Nothing }
    where
      p :: Proxy (Array a) -> Proxy a
      p _ = Proxy

instance hasTypeEither :: (HasType e, HasType a) => HasType (Either e a) where
  getType x = Type { n: "Either", p: [getType (p1 x), getType (p2 x)], o: Nothing }
    where
      p1 :: Proxy (Either e a) -> Proxy e
      p1 _ = Proxy
      p2 :: Proxy (Either e a) -> Proxy a
      p2 _ = Proxy

instance hasTypeTuple2 :: (HasType t1, HasType t2) => HasType (Tuple t1 t2) where
  getType x = Type { n: "Tuple", p: [getType (p1 x), getType (p2 x)], o: Nothing }
     where
       p1 :: Proxy (Tuple t1 t2) -> Proxy t1
       p1 _ = Proxy
       p2 :: Proxy (Tuple t1 t2) -> Proxy t2
       p2 _ = Proxy

instance hasTypeTuple3 :: (HasType t1, HasType t2, HasType t3) => HasType (Tuple t1 (Tuple t2 t3)) where
  getType x = Type { n: "Tuple", p: [getType (p1 x), getType (p2 x), getType (p3 x)], o: Nothing }
     where
       p1 :: Proxy (Tuple t1 (Tuple t2 t3)) -> Proxy t1
       p1 _ = Proxy
       p2 :: Proxy (Tuple t1 (Tuple t2 t3)) -> Proxy t2
       p2 _ = Proxy
       p3 :: Proxy (Tuple t1 (Tuple t2 t3)) -> Proxy t3
       p3 _ = Proxy
