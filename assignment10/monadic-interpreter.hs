
import Data.Map as M

data Expr = Num Integer
          | Id String
          | Add Expr Expr
  deriving(Show, Eq)

type Env = M.Map String Integer

newtype Reader e a = R { runReader :: e -> a }

instance Functor (Reader e) where
  f `fmap` (R e) = R (f . e)

instance Applicative (Reader e) where
  pure = return
  (R f) <*> (R e) = R (\ r -> f r (e r))

instance Monad (Reader e) where
  return a = R $ const a
  m >>= k  = R $ \ r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id


eval :: Expr -> Reader Env Integer
eval (Num i) = return i
eval (Id s)  = (ask >>= \ x -> return $ x M.! s)
eval (Add l r) = (eval l >>= (\ lv -> (eval r >>= \ rv -> return $ lv + rv)))

-- TODO:
-- inline return
evalInlineReturn (Num i) =
evalInlineReturn (Id s)  =
evalInlineReturn (Add l r) =

-- inline ask
evalInlineAsk (Num i) =
evalInlineAsk (Id s)  =
evalInlineAsk (Add l r) =

-- inline bind
evalInlineBind (Num i) =
evalInlineBind (Id s)  =
evalInlineBind (Add l r) =

-- remove Reader
evalDesugar (Num i) =
evalDesugar (Id s)  =
evalDesugar (Add l r) =

-- simplify lambda
evalFinalSimplified (Num i) =
evalFinalSimplified (Id s)  =
evalFinalSimplified (Add l r) =
