-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Cpp.Optimizer
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module optimizes code in the simplified-C++11 intermediate representation.
--
-- The following optimizations are supported:
--
--  * Collapsing nested blocks
--
--  * Tail call elimination
--
--  * Inlining of (>>=) and ret for the Eff monad
--
--  * Removal of unnecessary thunks
--
--  * Eta conversion
--
--  * Inlining variables
--
--  * Inline Prelude.($), Prelude.(#), Prelude.(++), Prelude.(!!)
--
--  * Inlining primitive C++11 operators
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.Cpp.Optimizer (
    optimize
) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Optimizer.Blocks
import Language.PureScript.CodeGen.Cpp.Optimizer.Common
import Language.PureScript.CodeGen.Cpp.Optimizer.Inliner
import Language.PureScript.CodeGen.Cpp.Optimizer.MagicDo
import Language.PureScript.CodeGen.Cpp.Optimizer.TCO
import Language.PureScript.CodeGen.Cpp.Optimizer.Uncurry
import Language.PureScript.CodeGen.Cpp.Optimizer.Unused

-- |
-- Apply a series of optimizer passes to simplified C++11 code
--
optimize :: MonadSupply m => NamesMap -> Cpp -> m Cpp
optimize nm cpp = do
  cpp' <- untilFixedPoint (inlineFnComposition .
                           inlineUnsafePartial .
                           tidyUp .
                           applyAll
    [ inlineCommonValues
    , inlineCommonOperators
    ]) cpp
  untilFixedPoint (return . removeCurrying nm) . toAutoVars . tidyUp . tco . magicDo $ cpp'
  where
  tidyUp :: Cpp -> Cpp
  tidyUp = applyAll
    [ collapseNestedBlocks
    , collapseCtorChecksToSwitch
    , collapseNestedIfs
    , collapseIfElses
    , removeCodeAfterReturnStatements
    , removeCodeAfterContinueStatements
    , removeUnusedArg
    , unThunk
    , etaConvert
    , evaluateIifes
    , inlineVariables
    ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
