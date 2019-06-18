module Language where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (List)
import qualified Data.Map as M
import Data.Text

import Parser
import Types

nil :: Term
nil = List []

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
  | MacroArgError Term
  | SFIllegalArgs SpecialForm [Term]
  | ParseError LexerParserError
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
  , MonadIO m -- file opening
  )

specialFormsEnv :: Env
specialFormsEnv = Env $ M.fromList $ fmap (bimap Sym SpecialForm)
  [ ("lambda", Lambda)
  , ("mac", Mac)
  , ("quote", Quote)
  , ("quasiquote", Quasiquote)
  , ("unquote", Unquote)
  , ("cons", Cons)
  , ("car", Car)
  , ("cdr", Cdr)
  , ("if", If)
  , ("def", Def)
  , ("eval", Eval)
  , ("apply", Apply)
  , ("load-file", LoadFile)
  ]

evalInterpreter :: MonadIO m => ReaderT Env (ExceptT InterpreterError (StateT InterpreterState m)) a
  -> m (Either InterpreterError a)
evalInterpreter ma = evalStateT (runExceptT (runReaderT ma specialFormsEnv)) emptyState

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
eval (String s) = pure (String s)
eval e = throwError (Can'tEval e)

call :: MonadInterpreter m => Term -> [Term] -> m Term
call (SpecialForm sf) args = callSF sf args
call (Function lexicalEnv params body) args = callFun lexicalEnv params args body
call (Macro lexicalEnv params body) args = eval =<< macroExpand lexicalEnv params args body
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
callSF Mac [params, body] = do
  funparams <- case params of
    List listParams -> ParamsList <$>
      traverse (\t -> mbError (MacroArgNotSymbol t) (t ^? _Symbol)) listParams
    Symbol varargs -> pure (Varargs varargs)
    _ -> throwError (MacroArgError params)
  lexicalEnv <- ask
  pure (Macro lexicalEnv funparams body)
callSF Quote [arg] = pure arg
callSF Quote args = pure (List args)
callSF Quasiquote [arg] = quasiquote arg
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
callSF Eval [t] = do
  eval =<< eval t
callSF Apply [f, args] = do
  eval f >>= \case
    Function lexicalEnv params body -> do
      eval args >>= \case
        List args' -> callFunNoEvalArgs lexicalEnv params args' body
        _ -> throwError (SFIllegalArgs Apply [f, args])
    Macro lexicalEnv params body -> case args of
      List args' -> eval =<< macroExpand lexicalEnv params args' body
      _ -> throwError (SFIllegalArgs Apply [f, args])
    _ -> throwError (SFIllegalArgs Apply [f, args])
callSF LoadFile [path] = do
  eval path >>= \case
    String filePath -> List <$> evalFile filePath
    _ -> throwError (SFIllegalArgs LoadFile [path])
callSF sf args = throwError (SFIllegalArgs sf args)

quasiquote :: forall m. MonadInterpreter m => Term -> m Term
quasiquote (List [Symbol (Sym "unquote"), arg]) = eval arg
quasiquote (List (Symbol (Sym "unquote"):args)) = throwError (SFIllegalArgs Unquote args)
quasiquote (List args) = List <$> mapM quasiquote args
quasiquote x = pure x

callFun :: MonadInterpreter m => Env -> Funparams -> [Term] -> Term -> m Term
callFun lexicalEnv params args body = do
  args' <- traverse eval args
  callFunNoEvalArgs lexicalEnv params args' body

callFunNoEvalArgs :: MonadInterpreter m => Env -> Funparams -> [Term] -> Term -> m Term
callFunNoEvalArgs lexicalEnv (ParamsList params) args body = do
  local (const (insertsEnv params args lexicalEnv)) (eval body)
callFunNoEvalArgs lexicalEnv (Varargs params) args body = do
  local (const (insertEnv params (List args) lexicalEnv)) (eval body)


macroExpand :: MonadInterpreter m => Env -> Funparams -> [Term] -> Term -> m Term
macroExpand lexicalEnv (ParamsList params) args body =
  local (const (insertsEnv params args lexicalEnv)) (eval body)
macroExpand lexicalEnv (Varargs params) args body =
  local (const (insertEnv params (List args) lexicalEnv)) (eval body)

readEval :: MonadInterpreter m => Text -> m [Term]
readEval s = do
  terms <- case parseTerms s of
    Right x -> pure x
    Left err -> throwError (ParseError err)
  traverse eval terms

evalFile :: MonadInterpreter m => FilePath -> m [Term]
evalFile file = readEval . pack =<< liftIO (readFile file)
