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
  deriving Show

newtype Symbol = Sym {unSymbol :: String}
  deriving (Eq, Ord, Show)

data Funparams
  = ParamsList [Symbol]
  | Varargs Symbol
  deriving Show

data Term
  = List [Term]
  | Symbol Symbol
  | String String
  | SpecialForm SpecialForm
  | Function Env Funparams Term
  | Macro Funparams Term
  deriving Show

newtype Env = Env {_unEnv :: M.Map Symbol Term}
  deriving Show
makeLenses ''Env
