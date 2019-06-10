module Language where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens hiding (List, _Cons)
import Data.Map as M
import Data.String

data SpecialForm
  = Defmacro
  | Asm
  | Quote
  | List
  | Eval
  deriving Show

data Term
  = Cons [Term]
  | Symbol String
  deriving Show
makePrisms ''Term

instance IsString Term where
  fromString = Symbol

type Symbol = String

data Macro = Macro
  { arglist :: [Symbol]
  , body :: Term
  }
  deriving Show

data InterpState = InterpState
  { _macros :: Map String Macro
  }
  deriving Show
makeLenses ''InterpState

data InterpError
  = IErrBadDefMacro [Term]
  | IErrUndefinedSymbol Symbol
  | IErrMacroWrongNumArgs Macro [Term]
  | IErrEvalWrongNumArgs [Term]
  deriving Show

type MonadInterpreter m = (MonadState InterpState m, MonadError InterpError m)

runInterpreter m = runExcept (runStateT m (InterpState M.empty))
evalInterpreter m = runExcept (evalStateT m (InterpState M.empty))

eval :: MonadInterpreter m => Term -> m Term
eval (Cons (f:args)) = case f of
  Symbol sym -> applySym sym args
  t -> do
    t' <- eval t
    eval (Cons (t:args))
eval x = error $ show x

applySym (toSF -> Just sf) args = applySF sf args
applySym sym args = do
  st <- get
  case M.lookup sym (st ^. macros) of
    Just macro -> eval =<< macroexpand macro args
    Nothing -> throwError (IErrUndefinedSymbol sym)

subst :: [Symbol] -> [Term] -> Term -> Term
subst ss xs (Cons ts) = Cons $ subst ss xs <$> ts
subst (s:ss) (x:xs) (Symbol s') | s == s' = x
                                | otherwise = subst ss xs (Symbol s')
subst [] [] (Symbol s') = (Symbol s')

--TODO: hygenic macros
--TODO: length-indexed arglists
macroexpand :: MonadInterpreter m => Macro -> [Term] -> m Term
macroexpand m@(Macro arglist body) args
  | length arglist == length args = pure (subst arglist args body)
  | otherwise = throwError (IErrMacroWrongNumArgs m args)

toSF :: Symbol -> Maybe SpecialForm
toSF "defmacro" = Just Defmacro
toSF "asm" = Just Asm
toSF "quote" = Just Quote
toSF "list" = Just List
toSF "eval" = Just Eval
toSF _ = Nothing

applySF Defmacro args = defmacro args
applySF Asm args = mkAsm args
applySF Quote args = pure $ Cons args
applySF List args = Cons <$> traverse eval args
applySF Eval [x] = eval x
applySF Eval args = throwError (IErrEvalWrongNumArgs args)

mkAsm args = pure (Cons args)

addmacro :: MonadState InterpState m => Symbol -> Macro -> m ()
addmacro name macro = modify (over macros (M.insert name macro))

unit = Cons []

checkArglist :: Term -> Maybe [Symbol]
checkArglist = traverse (^? _Symbol) <=< (^? _Cons)

defmacro :: MonadInterpreter m => [Term] -> m Term
defmacro [Symbol name, (checkArglist -> Just arglist), body]
  = unit <$ addmacro name (Macro arglist body)
defMacro ts = throwError (IErrBadDefMacro ts)



exampleDefmacro = Cons ["defmacro", "revFuncall", Cons ["a", "b"], Cons ["b", "a"]]
useFuncall = Cons ["revFuncall", "x", "f"]
exampleAsm = Cons ["asm", "xor", "eax", "ebx"]
multipleAsm = Cons ["list", exampleAsm, exampleAsm]
defClearRegister = Cons ["defmacro", "clearRegister", Cons ["register"], Cons ["asm", "xor", "register", "register"]]
clearEaxEbx = Cons ["list", Cons ["clearRegister", "eax"], Cons ["clearRegister", "ebx"]]
defIf = Cons ["defmacro", "if", Cons ["cond", "true", "false"]
             , Cons [ "list"
                    , "cond"
                    , Cons ["asm", "jnz", "ifFalse"]
                    , "true"
                    , Cons ["asm", "jmp", "ifEnd"]
                    , Cons ["asm", "ifFalse:"]
                    , "false"
                    , Cons ["asm", "ifEnd:"]]]
ifTest = Cons [ "if"
              , Cons ["asm", "cmp", "eax", "ebx"]
              , Cons ["clearRegister", "eax"]
              , Cons ["clearRegister", "ebx"]
              ]
