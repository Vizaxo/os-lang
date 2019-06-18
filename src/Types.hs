module Types where

import Control.Lens
import qualified Data.Map as M

data SpecialForm
  = Lambda
  | Mac
  | Quote
  | Quasiquote
  | Unquote
  | Cons
  | Car
  | Cdr
  | If
  | Def
  | Eval
  | Apply
  | LoadFile
  | EqP
  deriving (Eq, Show)

newtype Symbol = Sym {unSymbol :: String}
  deriving (Eq, Ord, Show)

data Funparams
  = ParamsList [Symbol]
  | Varargs Symbol
  deriving (Eq, Show)

data Term
  = List [Term]
  | Symbol Symbol
  | String String
  | SpecialForm SpecialForm
  | Function Env Funparams Term
  | Macro Env Funparams Term
  deriving (Eq, Show)

newtype Env = Env {_unEnv :: M.Map Symbol Term}
  deriving (Eq, Show)
makeLenses ''Env
