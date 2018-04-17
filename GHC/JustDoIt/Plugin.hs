{-# LANGUAGE CPP, TupleSections #-}
module GHC.JustDoIt.Plugin ( plugin )
where

-- external
import Data.Maybe
import Control.Monad

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

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = lookupJDITyCon
           , tcPluginSolve = solveJDI
           , tcPluginStop  = const (return ())
           }

lookupJDITyCon :: TcPluginM (Class, Class)
lookupJDITyCon = do
    Found _ md   <- findImportedModule jdiModule Nothing
    jdiTcNm <- lookupOrig md (mkTcOcc "JustDoIt")
    workingTcNm <- lookupOrig md (mkTcOcc "Working")
    (,) <$> tcLookupClass jdiTcNm
        <*> tcLookupClass workingTcNm
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


solveJDI :: (Class, Class) -- ^ JDI's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI (jdi, w) [] [] [ws]
  | Just (pred, [c]) <- getJDI jdi ws
  = let (dest, loc) = getLoc ws
     in pure $ TcPluginOk [] [ CNonCanonical $ CtDerived c loc
                             , CNonCanonical $ CtDerived pred loc
                             , CNonCanonical $ CtDerived (mkClassPred w []) loc
                             , ws
                             ]
solveJDI (_, w) [] [ws] _
  | Just _ <- getJDI w ws
  = pprPanic "solved it" $ ppr w

-- solveJDI jdiCls [] [z] [ws]
--   | Just c <- getJDI jdiCls ws
--   = pprPanic "solved it" $ ppr z

solveJDI _ g d w =
  pprPanic "did not solve it" $ ppr [g, d, w]

