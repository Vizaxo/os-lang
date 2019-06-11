module Language where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M

data Term
  = Cons [Term]
  | Symbol Symbol
  deriving Show

newtype Symbol = Sym {unSymbol :: String}
  deriving (Eq, Ord, Show)

newtype Env = Env {unEnv :: Map Symbol Term}
  deriving Show

emptyEnv :: Env
emptyEnv = Env (M.empty)

data InterpreterError
  = SymbolUndefined Symbol
  | IllegalCall Term [Term]
  | Can'tEval Term
  deriving Show

type MonadInterpreter m = (MonadReader Env m, MonadError InterpreterError m)

evalInterpreter ma = runExcept (runReaderT ma emptyEnv)

mbError :: MonadError err m => err -> Maybe a -> m a
mbError err Nothing = throwError err
mbError err (Just x) = pure x

lookupEnv :: MonadInterpreter m => Symbol -> m Term
lookupEnv s = do
  env <- ask
  case M.lookup s (unEnv env) of
    Nothing -> throwError (SymbolUndefined s)
    Just t -> pure t

eval :: MonadInterpreter m => Term -> m Term
eval (Cons (x:xs)) = flip call xs =<< eval x
eval (Symbol s) = lookupEnv s
eval e = throwError (Can'tEval e)

call :: MonadInterpreter m => Term -> [Term] -> m Term
call op args = throwError (IllegalCall op args)
