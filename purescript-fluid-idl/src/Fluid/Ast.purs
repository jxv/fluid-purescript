module Fluid.Ast where

import Prelude (Unit, ($), map)
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Data.Either (Either(..))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple as T
import Fluid.Types

data Ast
  = Ast'Ref Ref
  | Ast'If If
  | Ast'Iflet Iflet
  | Ast'Get Get
  | Ast'Set Set
  | Ast'Define Define
  | Ast'Match Match
  | Ast'List List
  | Ast'Tuple Tuple
  | Ast'Enumeral Enumeral
  | Ast'Const Const

class ToAst a where
  toAst :: a -> Ast

instance unitToAst :: ToAst Unit where
  toAst _ = Ast'Const Const'Null

instance booleanToAst :: ToAst Boolean where
  toAst b = Ast'Const (Const'Bool b)

instance stringToAst :: ToAst String where
  toAst s = Ast'Const (Const'String s)

instance intToAst :: ToAst Int where
toAst x = Ast'Const (Const'Number (Int.toNumber x))

instance numberToAst :: ToAst Number where
  toAst x = Ast'Const (Const'Number x)

instance listToAst :: ToAst a => ToAst (Array a) where
  toAst xs = Ast'List $ List { list: map toAst xs }

instance maybeToAst :: ToAst a => ToAst (Maybe a) where
  toAst Nothing = Ast'Const Const'Null
  toAst (Just x) = toAst x

instance eitherToAst :: (ToAst a, ToAst b) => ToAst (Either a b) where
  toAst (Left a) = Ast'Enumeral $ Enumeral { tag: "Left", m: Just $ StrMap.fromFoldable [T.Tuple "left" (toAst a)] }
  toAst (Right a) = Ast'Enumeral $ Enumeral { tag: "Right", m: Just $ StrMap.fromFoldable [T.Tuple "right" (toAst a)] }

instance tuple2ToAst :: (ToAst a, ToAst b) => ToAst (T.Tuple a b) where
  toAst (T.Tuple t1 t2) = Ast'Tuple $ Tuple { tuple: [toAst t1, toAst t2] }

--

data Ref = Ref
  { symbol :: Symbol
  }

data If = If
  { cond :: Ast
  , onTrue :: Ast
  , onFalse :: Ast
  }

data Iflet = Iflet
  { symbol :: Symbol
  , option :: Ast
  , some :: Ast
  , none :: Ast
  }

data Get = Get
  { path :: Array String
  , val :: Ast
  }

data Set = Set
  { path :: Array String
  , src :: Ast
  , dest :: Ast
  }

data Define = Define
  { var :: Symbol
  , expr :: Ast
  }

data MatchCase
  = MatchCase'Tag EnumeralName Ast
  | MatchCase'Members EnumeralName Symbol Ast

data Match = Match
  { enumeral :: Ast
  , cases :: Array MatchCase
  }

data List = List
  { list :: Array Ast
  }

data Tuple = Tuple
  { tuple :: Array Ast
  }

data Enumeral = Enumeral
  { tag :: EnumeralName
  , m :: Maybe (StrMap Ast)
  }
