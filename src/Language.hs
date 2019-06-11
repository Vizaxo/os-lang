module Language where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Map as M

data SpecialForm
  = Lambda
  deriving Show

newtype Symbol = Sym {unSymbol :: String}
  deriving (Eq, Ord, Show)

data Term
  = Cons [Term]
  | Symbol Symbol
  | SpecialForm SpecialForm
  | Function [Symbol] Term
  deriving Show
makePrisms ''Term

newtype Env = Env {unEnv :: Map Symbol Term}
  deriving Show

emptyEnv :: Env
emptyEnv = Env (M.empty)

data InterpreterError
  = SymbolUndefined Symbol
  | IllegalCall Term [Term]
  | Can'tEval Term
  | LambdaArgNotSymbol Term
  | LambdaIllegalArgs [Term]
  deriving Show

type MonadInterpreter m = (MonadReader Env m, MonadError InterpreterError m)

specialFormsEnv :: Env
specialFormsEnv = Env $ M.fromList $ fmap (bimap Sym SpecialForm)
  [ ("lambda", Lambda)
  ]

evalInterpreter ma = runExcept (runReaderT ma specialFormsEnv)

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
call (SpecialForm sf) args = callSF sf args
call op args = throwError (IllegalCall op args)

callSF :: MonadInterpreter m => SpecialForm -> [Term] -> m Term
callSF Lambda [Cons args, body] = do
  --args' <- mbError LambdaArgNotSymbol (args ^? (mapped._Symbol))
  args' <- traverse (\t -> mbError (LambdaArgNotSymbol t) (t ^? _Symbol)) args
  pure (Function args' body)
callSF Lambda args = throwError (LambdaIllegalArgs args)
