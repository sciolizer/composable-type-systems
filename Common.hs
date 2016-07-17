-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Common where

import Control.Applicative

-- or maybe just pointed?
class (Applicative f) => Boolean f exp where
  true :: f exp
  false :: f exp
  ifThenElse :: exp -> exp -> exp -> f exp

class BooleanType f tipe where
  boolean :: f tipe

{-
class TypeVariable tipe where
  variable :: String -> tipe
  viewVariable :: tipe -> Maybe String
-}
-- todo: shrink to Applicative
class (Applicative f, Monad f) => TypeEnvironment f exp tipe | f -> exp tipe where
  typeCheck :: exp -> f tipe
  eq :: tipe -> tipe -> f ()

typeCheckBool :: (TypeEnvironment f exp tipe, Boolean exp, BooleanType tipe) => exp -> f tipe
typeCheckBool e =
  case e of
    (viewBoolean -> Just _) -> pure boolean
    (viewIfThenElse -> Just (cond, thenBranch, elseBranch)) -> do
      (viewBooleanType -> True) <- typeCheckBool cond
      tb <- typeCheck thenBranch
      eb <- typeCheck elseBranch
      tb `eq` eb
      return tb
    {-

class (TypeEnvironment f) => TypeCheck f e t | e -> f t where
  typeCheck :: e -> f t


instance (TypeEnvironment f, Boolean e, IfThenElse e, BoolType t) => TypeCheck f e t where
  typeCheck = undefined
  {-
typeCheck e =
  case e of
    ifThenElse a b c -> do
      bType <- typeCheck a
      when (bType /= bool) fail "jkl"
      thenType <- typeCheck b
      elseType <- typeCheck c
      when (thenType /= elseType) fail "fjdksl"
      return thenType
      -}
      -}