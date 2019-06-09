module Language where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Map as M

data SpecialForm
  = Defmacro
  | Asm
  deriving Show

data Term
  = Cons [Term]
  | Symbol String
  deriving Show
makePrisms ''Term

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
  deriving Show

type MonadInterpreter m = (MonadState InterpState m, MonadError InterpError m)

runMonadInterpreter m = runExcept (runStateT m (InterpState M.empty))

eval :: MonadInterpreter m => Term -> m Term
eval (Cons (f:args)) = case f of
  Symbol sym -> applySym sym args
  t -> do
    t' <- eval t
    eval (Cons (t:args))
eval x = undefined

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
toSF _ = Nothing

applySF Defmacro args = defmacro args
applySF Asm args = mkAsm args

mkAsm = undefined

addmacro :: MonadState InterpState m => Symbol -> Macro -> m ()
addmacro name macro = modify (over macros (M.insert name macro))

unit = Cons []

checkArglist :: Term -> Maybe [Symbol]
checkArglist (Cons args) = traverse (^? _Symbol) args
checkArglist _ = Nothing

defmacro :: MonadInterpreter m => [Term] -> m Term
defmacro [Symbol name, (checkArglist -> Just arglist), body]
  = unit <$ addmacro name (Macro arglist body)
defMacro ts = throwError (IErrBadDefMacro ts)



exampleDefmacro = Cons [Symbol "defmacro", Symbol "revFuncall", Cons [Symbol "a", Symbol "b"], Cons [Symbol "b", Symbol "a"]]
useFuncall = Cons [Symbol "revFuncall", Symbol "x", Symbol "f"]
