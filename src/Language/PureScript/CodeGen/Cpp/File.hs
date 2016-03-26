-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.File
-- Copyright   :  (c) 2013-15 Phil Freeman, Andy Arvanitis, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Andy Arvanitis <andy.arvanitis@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- File generation utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.CodeGen.Cpp.File where

import Data.Char (isLower, isUpper)
import Data.Tuple (swap)
import Data.Maybe
import qualified Data.Graph as G

import Language.PureScript.CodeGen.Cpp.AST
import Language.PureScript.CodeGen.Cpp.Types
import Language.PureScript.Names
import qualified Language.PureScript.Pretty.Cpp as P

---------------------------------------------------------------------------------------------------
instances :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
instances = depSortInstances . catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go cpp@(CppStruct (c:_) _ _) | isLower c = Just cpp
  go _ = Nothing

---------------------------------------------------------------------------------------------------
depSortInstances :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
depSortInstances cpps = reverse . catMaybes $
                            flip lookup vertexCpps <$>
                                G.topSort (G.buildG (1, length cpps) (concatMap findEdges cpps))
  where
  findEdges :: Cpp -> [G.Edge]
  findEdges cpp@(CppStruct _ supers _)
    | Just thisVertex <- lookup cpp vertexCpps' =
      concatMap (go thisVertex) $ filter (isLower . head) supers
  findEdges _ = []

  go :: G.Vertex -> String -> [G.Edge]
  go thisVertex name
    | Just depVertex <- lookup name vertexes = [(thisVertex, depVertex)]
  go _ _ = []

  vertexes :: [(String, G.Vertex)]
  vertexes = zip (getName <$> cpps) [1 ..]

  vertexCpps :: [(G.Vertex, Cpp)]
  vertexCpps = zip [1 ..] cpps

  vertexCpps' :: [(Cpp, G.Vertex)]
  vertexCpps' = swap <$> vertexCpps

  getName :: Cpp -> String
  getName (CppStruct name _ _) = name
  getName cpp = error $ "Wrong kind of Cpp value! " ++ show cpp

---------------------------------------------------------------------------------------------------
toHeader :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeader = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toHeader cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppUseNamespace{}) = Just cpp
  go cpp@(CppStruct (c:_) _ _) | isUpper c = Just cpp
  go (CppFunction _ _ _ qs _)
    | CppInline `elem` qs = Nothing
  go (CppFunction _ [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp _ [_])])) | atyp == Just CppAuto
    = Nothing
  go (CppFunction name args rtyp qs _) =
    let args' = (\(_,t) -> ("", t)) <$> args in
    Just (CppFunction name args' rtyp qs CppNoOp)
  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) =
    Nothing
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just _)) =
    Just $ CppVariableIntroduction (name, Just $ CppAny [CppConst]) [CppExtern] Nothing
  go (CppComment comms cpp')
    | Just cpp <- go cpp' = Just $ case cpp of CppFunction {} -> cpp
                                               _ -> CppComment comms cpp
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toHeaderFns :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toHeaderFns = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toHeaderFns cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppFunction _ _ _ qs _)
    | CppInline `elem` qs = Just cpp
  go (CppFunction name [(_, atyp)] _ _ (CppBlock [CppReturn (CppApp cpp [_])]))
    | atyp == Just CppAuto
    = Just (CppVariableIntroduction (name, Nothing) [] (Just cpp))
  go cpp@(CppVariableIntroduction _ _ (Just CppNumericLiteral {})) =
    Just cpp
  go cpp@(CppVariableIntroduction _ _ (Just CppStringLiteral {})) =
    Just cpp
  go cpp@(CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) =
    Just cpp

  go (CppComment comms cpp') | Just cpp <- go cpp' = Just (CppComment comms cpp)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBodyDecl :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBodyDecl = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppComment comms cpp') | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

---------------------------------------------------------------------------------------------------
toBody :: [Cpp] -> [Cpp]
---------------------------------------------------------------------------------------------------
toBody = catMaybes . map go
  where
  go :: Cpp -> Maybe Cpp
  go (CppNamespace name cpps)
    | cpps'@(_:_) <- toBody cpps = Just (CppNamespace name cpps')
    | otherwise = Nothing
  go cpp@(CppUseNamespace{}) = Just cpp
  go (CppFunction _ _ _ qs _)
    | CppInline `elem` qs = Nothing
  go cpp@(CppFunction {}) = Just cpp
  go (CppVariableIntroduction _ _ (Just CppNumericLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppStringLiteral {})) =
    Nothing
  go (CppVariableIntroduction _ _ (Just CppBooleanLiteral {})) =
    Nothing
  -- Generate thunks for top-level values
  go (CppVariableIntroduction (name, _) _ (Just cpp)) =
    Just $ CppVariableIntroduction (name, Just $ CppAny [CppConst]) [] (Just lambda)
    where
    val = CppVariableIntroduction ("_value_", Just $ CppAny [CppConst]) [CppStatic] (Just $ addCaptures cpp)
    block = CppBlock [val, CppReturn (CppVar "_value_")]
    lambda = CppLambda [] [("", Just $ thunkMarkerType)] (Just $ CppAny [CppConst, CppRef]) block
    addCaptures :: Cpp -> Cpp
    addCaptures (CppObjectLiteral objs) = CppObjectLiteral (objlam <$> objs)
      where
      objlam :: (String, Cpp) -> (String, Cpp)
      objlam (name', (CppLambda _ args rty body)) = (name' , CppLambda [] args rty $ addCaptures body)
      objlam obj = obj
    addCaptures (CppApp (CppLambda _ [] rty body) []) = CppApp (CppLambda [] [] rty (addCaptures body)) []
    addCaptures cpps' = everywhereOnCpp addCapture cpps'
      where
      addCapture :: Cpp -> Cpp
      addCapture (CppLambda _ args rty body) = maybeRemCaps $ CppLambda [CppCaptureAll] args rty body
      addCapture cpp' = cpp'
  go (CppComment comms cpp') | Just commented <- go cpp' = Just (CppComment comms commented)
  go _ = Nothing

-------------------------------------------------------------------------------------------------
maybeRemCaps :: Cpp -> Cpp
-------------------------------------------------------------------------------------------------
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn CppNumericLiteral{}]))
  = CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn CppStringLiteral{}]))
  = CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn CppCharLiteral{}]))
  = CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn CppBooleanLiteral{}]))
  = CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn CppAccessor{}]))
  = CppLambda [] args rtyp body
maybeRemCaps (CppLambda [CppCaptureAll] args rtyp body@(CppBlock [CppReturn (CppVar "unit")])) -- TODO: not really safe
  = CppLambda [] args rtyp body
maybeRemCaps cpp = cpp

---------------------------------------------------------------------------------------------------

fileBegin :: ModuleName -> String -> [Cpp]
fileBegin mn suffix = [CppRaw ("#ifndef " ++ fileModName mn suffix),
                       CppRaw ("#define " ++ fileModName mn suffix)]

fileEnd :: ModuleName -> String -> [Cpp]
fileEnd mn suffix = [CppRaw ("#endif // " ++ fileModName mn suffix)]

fileModName :: ModuleName -> String -> String
fileModName mn suffix = P.dotsTo '_' (runModuleName mn ++ '_' : suffix)

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

nativeMain :: Cpp
nativeMain = CppFunction "main"
               [ ([], Just $ CppPrimitive "int")
               , ([], Just $ CppPrimitive "char *[]")
               ]
               (Just $ CppPrimitive "int")
               []
               (CppBlock [ CppUseNamespace "Main"
                         , CppApp (CppApp (CppAccessor (CppVar "main") (CppVar "Main"))
                                          [CppVar unthunkMarkerValue])
                                  []
                         , CppReturn (CppNumericLiteral (Left 0))
                         ])
