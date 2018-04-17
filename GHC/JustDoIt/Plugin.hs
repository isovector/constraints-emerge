{-# LANGUAGE CPP, TupleSections #-}
module GHC.JustDoIt.Plugin ( plugin )
where

-- external
import Data.Maybe
import Control.Monad
import Data.List (partition)

import TcType (TcPredType)
-- GHC API
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence
import TcPluginM
import TcRnTypes
import Class
import CoreUtils
import MkCore
import TyCon
import Type
import CoreSyn
import Outputable
import InstEnv (InstEnvs (..), ClsInst (..), lookupUniqueInstEnv, lookupInstEnv)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = lookupJDITyCon
           , tcPluginSolve = solveJDI
           , tcPluginStop  = const (return ())
           }

data JDI = JDI
  { jdiJDI     :: Class
  , jdiWorking :: Class
  , jdiFail    :: Class
  , jdiSucceed :: Class
  }


lookupJDITyCon :: TcPluginM JDI
lookupJDITyCon = do
    Found _ md   <- findImportedModule jdiModule Nothing
    jdiTcNm <- lookupOrig md (mkTcOcc "JustDoIt")
    workingTcNm <- lookupOrig md (mkTcOcc "Working")
    failTcNm <- lookupOrig md (mkTcOcc "AlwaysFail")
    succeedTcNm <- lookupOrig md (mkTcOcc "Succeed")
    JDI
        <$> tcLookupClass jdiTcNm
        <*> tcLookupClass workingTcNm
        <*> tcLookupClass failTcNm
        <*> tcLookupClass succeedTcNm
  where
    jdiModule  = mkModuleName "GHC.JustDoIt"

-- wrap :: Class -> CoreExpr -> EvTerm
-- wrap cls = EvExpr . appDc
--   where
--     tyCon = classTyCon cls
--     dc = tyConSingleDataCon tyCon
--     appDc x = mkCoreConApps dc [Type (exprType x), x]

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint cls ct = do
    (cls', [t]) <- getClassPredTys_maybe (ctPred ct)
    guard (cls' == cls)
    return (ct, t)


getJDI :: Class -> Ct -> Maybe (TcPredType, [Type])
getJDI c (CNonCanonical (CtWanted pred _ _)) =
  getJDI' c pred
getJDI c (CNonCanonical (CtDerived pred _)) =
  getJDI' c pred
getJDI c (CNonCanonical (CtGiven pred _ _)) =
  getJDI' c pred

getJDI' :: Class -> Type -> Maybe (Type, [Type])
getJDI' c pred =
  case splitTyConApp_maybe pred of
    Just (x, preds) ->
      case x == classTyCon c of
        True -> Just (pred, preds)
        False -> Nothing
    _ -> Nothing

getLoc :: Ct -> (TcEvDest, CtLoc)
getLoc (CNonCanonical (CtWanted _ b c)) = (b, c)

isJDI :: Class -> Ct -> Bool
isJDI c = isJust . getJDI c


solveJDI :: JDI
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI (JDI jdi w _ _) _ [] [ws]
  | Just (pred, [c]) <- getJDI jdi ws
  = let (dest, loc) = getLoc ws
     in pure $ TcPluginOk [] [ CNonCanonical $ CtDerived c loc
                             , CNonCanonical $ CtDerived pred loc
                             , CNonCanonical $ CtDerived (mkClassPred w []) loc
                             , ws
                             ]

-- solved it
solveJDI (JDI j w _ succ) _ [ws] [z]
  | Just _ <- getJDI w ws
  , Just (a, [b]) <- getJDI j z
  = do
    let Just ok = splitTyConApp_maybe b
    myclass <- tcLookupClass (tyConName $ fst ok)
    envs <- getInstEnvs
    classdfun <- case lookupUniqueInstEnv envs myclass $ snd ok of
      Right (clsInst, _) -> pure $ is_dfun clsInst
        -- pprPanic "great" $ ppr clsInst
        -- pure $ TcPluginOk
        --   [ (EvLit $ EvNum 0, ws)
        --   , (EvDFunApp (is_dfun clsInst) [b] [], z)
        --   ]
        --   []
      Left err -> pprPanic "fuck1" $ ppr succ

    case lookupUniqueInstEnv envs succ [b] of
      Right (clsInst, _) ->
        pure $ TcPluginOk
          [ (EvLit $ EvNum 0, ws)
          , (EvDFunApp (is_dfun clsInst) [b] [EvDFunApp classdfun [] []], z)
          ]
          []
      Left err -> pprPanic "fuck2" $ ppr succ

-- -- recursive case
-- solveJDI jdi@(JDI j _ _ _) _ n@[a, b] [z]
--   | any (isJDI j) [z]
--   , any (isJDI j) n
--   = let (m, n') = partition (isJDI j) n
--      in solveJDI jdi [] m (z : n')

-- did not solve it
solveJDI (JDI j _ fail _) _ [a, b] [z]
  | any (isJDI j) [z]
  = do
    envs <- getInstEnvs
    case lookupUniqueInstEnv envs fail [] of
      Right (clsInst, _) ->
        pure $ TcPluginOk
          [ (EvLit $ EvNum 0, a)
          , (EvLit $ EvNum 0, b)
          , (EvDFunApp (is_dfun clsInst) [] [], z)
          ]
          []
      Left err -> pprPanic "fuck" $ ppr $ ie_global envs

-- lookupUniqueInstEnv :: InstEnvs -> Class -> [Type] -> Either MsgDoc (ClsInst, [Type])

-- solveJDI jdiCls [] [z] [ws]
--   | Just c <- getJDI jdiCls ws
--   = pprPanic "solved it" $ ppr z

-- solveJDI _ a b c = pprPanic "lame" $ ppr (a, b, c)

solveJDI _ a b c = pure $ TcPluginOk [] []

