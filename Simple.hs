module Simple where

import Control.Monad

data Exp
  = LiteralBoolean Bool
  | IfThenElse Exp Exp Exp
  | LiteralInt Int
  | Predecessor Exp
  | Successor Exp
  | IsZero Exp

data Type
  = TInt
  | TBool
  deriving (Eq)

typeCheck :: (Monad m) => Exp -> m Type
typeCheck e =
  case e of
    LiteralBoolean _ -> return TBool
    IfThenElse ee tb eb -> do
      TBool <- typeCheck ee
      ttb <- typeCheck tb
      teb <- typeCheck eb
      when (ttb /= teb) $ fail "mismatch"
      return ttb
