module Language where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (List)
import qualified Data.Map as M

data SpecialForm
  = Lambda
  | Mac
  | Quote
  | Cons
  | Car
  | Cdr
  | If
  | Def
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
  | SpecialForm SpecialForm
  | Function Env Funparams Term
  | Macro [Symbol] Term
  deriving Show

nil :: Term
nil = List []

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

makePrisms ''Term

data InterpreterError
  = SymbolUndefined Symbol
  | IllegalCall Term [Term]
  | Can'tEval Term
  | LambdaArgNotSymbol Term
  | LambdaArgError Term
  | MacroArgNotSymbol Term
  | SFIllegalArgs SpecialForm [Term]
  deriving Show

data InterpreterState = InterpreterState
  { _globalDefs :: M.Map Symbol Term
  }
makeLenses ''InterpreterState

emptyState :: InterpreterState
emptyState = InterpreterState M.empty

type MonadInterpreter m =
  ( MonadReader Env m
  , MonadError InterpreterError m
  , MonadState InterpreterState m
  )

specialFormsEnv :: Env
specialFormsEnv = Env $ M.fromList $ fmap (bimap Sym SpecialForm)
  [ ("lambda", Lambda)
  , ("mac", Mac)
  , ("quote", Quote)
  , ("cons", Cons)
  , ("car", Car)
  , ("cdr", Cdr)
  , ("if", If)
  , ("def", Def)
  ]

evalInterpreter :: ReaderT Env (ExceptT InterpreterError (State InterpreterState)) a
  -> Either InterpreterError a
evalInterpreter ma = evalState (runExceptT (runReaderT ma specialFormsEnv)) emptyState

mbError :: MonadError err m => err -> Maybe a -> m a
mbError err Nothing = throwError err
mbError err (Just x) = pure x

lookupEnv :: MonadInterpreter m => Symbol -> m Term
lookupEnv s = do
  env <- ask
  case M.lookup s (env ^. unEnv) of
    Nothing -> do
      globals <- view globalDefs <$> get
      case M.lookup s globals of
        Nothing -> throwError (SymbolUndefined s)
        Just t -> pure t
    Just t -> pure t

eval :: MonadInterpreter m => Term -> m Term
eval (List (x:xs)) = flip call xs =<< eval x
eval (Symbol s) = lookupEnv s
eval e = throwError (Can'tEval e)

call :: MonadInterpreter m => Term -> [Term] -> m Term
call (SpecialForm sf) args = callSF sf args
call (Function lexicalEnv params body) args = callFun lexicalEnv params args body
call (Macro params body) args = eval =<< macroExpand params args body
call op args = throwError (IllegalCall op args)

callSF :: MonadInterpreter m => SpecialForm -> [Term] -> m Term
callSF Lambda [params, body] = do
  --params' <- mbError LambdaArgNotSymbol (params ^? (mapped._Symbol))
  funparams <- case params of
    List listParams -> ParamsList <$>
      traverse (\t -> mbError (LambdaArgNotSymbol t) (t ^? _Symbol)) listParams
    Symbol varargs -> pure (Varargs varargs)
    _ -> throwError (LambdaArgError params)
  lexicalEnv <- ask
  pure (Function lexicalEnv funparams body)
callSF Mac [List args, body] = do
  args' <- traverse (\t -> mbError (MacroArgNotSymbol t) (t ^? _Symbol)) args
  pure (Macro args' body)
callSF Quote [arg] = pure arg
callSF Quote args = pure (List args)
callSF Cons [car, cdr] = do
  car' <- eval car
  eval cdr >>= \case
    List cdr' -> pure (List (car':cdr'))
    _ -> throwError (SFIllegalArgs Cons [car, cdr])
callSF Car [arg] = do
  eval arg >>= \case
    List (x:xs) -> pure x
    _ -> throwError (SFIllegalArgs Car [arg])
callSF Cdr [arg] = do
  eval arg >>= \case
    List (x:xs) -> pure (List xs)
    _ -> throwError (SFIllegalArgs Cdr [arg])
callSF If [c, t, f] = do
  eval c >>= \case
    List [] -> eval f --nil is false
    _ -> eval t
callSF Def [Symbol name, val] = do
  val' <- eval val
  modify (over globalDefs (M.insert name val'))
  pure nil
callSF sf args = throwError (SFIllegalArgs sf args)

callFun :: MonadInterpreter m => Env -> Funparams -> [Term] -> Term -> m Term
callFun lexicalEnv (ParamsList params) args body = do
  args' <- traverse eval args
  local (const (insertsEnv params args' lexicalEnv)) (eval body)
callFun lexicalEnv (Varargs params) args body = do
  args' <- traverse eval args
  local (const (insertEnv params (List args') lexicalEnv)) (eval body)


macroExpand :: MonadInterpreter m => [Symbol] -> [Term] -> Term -> m Term
macroExpand params args body =
  local (insertsEnv params args) (eval body)
