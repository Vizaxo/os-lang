module Language where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M

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

newtype Env = Env {_unEnv :: M.Map Symbol Term}
  deriving Show
makeLenses ''Env

emptyEnv :: Env
emptyEnv = Env (M.empty)

insertEnv :: Symbol -> Term -> Env -> Env
insertEnv s t = over unEnv (M.insert s t)

insertsEnv :: [Symbol] -> [Term] -> Env -> Env
insertsEnv [] _ e = e
insertsEnv _ [] e = e
insertsEnv (s:ss) (t:ts) e = insertEnv s t (insertsEnv ss ts e)

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
  case M.lookup s (env ^. unEnv) of
    Nothing -> throwError (SymbolUndefined s)
    Just t -> pure t

eval :: MonadInterpreter m => Term -> m Term
eval (Cons (x:xs)) = flip call xs =<< eval x
eval (Symbol s) = lookupEnv s
eval e = throwError (Can'tEval e)

call :: MonadInterpreter m => Term -> [Term] -> m Term
call (SpecialForm sf) args = callSF sf args
call (Function params body) args = callFun params args body
call op args = throwError (IllegalCall op args)

callSF :: MonadInterpreter m => SpecialForm -> [Term] -> m Term
callSF Lambda [Cons args, body] = do
  --args' <- mbError LambdaArgNotSymbol (args ^? (mapped._Symbol))
  args' <- traverse (\t -> mbError (LambdaArgNotSymbol t) (t ^? _Symbol)) args
  pure (Function args' body)
callSF Lambda args = throwError (LambdaIllegalArgs args)

callFun :: MonadInterpreter m => [Symbol] -> [Term] -> Term -> m Term
callFun params args body = do
  args' <- traverse eval args
  local (insertsEnv params args') (eval body)
