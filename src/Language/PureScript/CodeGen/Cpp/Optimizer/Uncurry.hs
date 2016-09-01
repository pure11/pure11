-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer.Uncurry
-- Copyright   :  (c) Andy Arvanitis 2016
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused variables
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Cpp.Optimizer.Uncurry where

import Prelude.Compat

import Language.PureScript.CodeGen.Cpp.AST
import qualified Language.PureScript.Types as T

type NamesMap = [(Cpp, Int)]

removeCurrying :: NamesMap -> Cpp -> Cpp
removeCurrying nm = everywhereOnCpp convert
  where
  convert cpp@(CppApp {})
    | (f, args@(_:_:_)) <- unApp cpp [],
      Just f' <- unCurried f,
      Just arity' <- lookup f' nm,
      arity' > 1 && arity' <= length args =
      foldl
        (\fn a -> CppApp fn [a])
        (CppApp f' (take arity' args))
        (drop arity' args)
    where
    unCurried :: Cpp -> Maybe Cpp
    unCurried (CppAccessor acc cpp)
      | Just cpp' <- unCurried cpp = Just $ CppAccessor acc cpp'
    unCurried (CppVar ('$':name)) = Just $ CppVar name
    unCurried _ = Nothing
  convert cpp = cpp

-------------------------------------------------------------------------------------------------
unApp :: Cpp -> [Cpp] -> (Cpp, [Cpp])
-------------------------------------------------------------------------------------------------
unApp (CppApp val args) args' = unApp val (args ++ args')
unApp other args' = (other, args')
