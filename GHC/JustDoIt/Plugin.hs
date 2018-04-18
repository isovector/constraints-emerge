{-# OPTIONS_GHC -Wall #-}

module GHC.JustDoIt.Plugin (plugin) where

import Control.Exception (throw)
import Control.Monad
import Data.Maybe
import Prelude hiding (pred)

import Class hiding (className)
import GHC (GhcException (..))
import InstEnv (ClsInst (..), lookupUniqueInstEnv)
import Module (mkModuleName)
import OccName (mkTcOcc)
import Outputable
import Plugins (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (EvDFunApp))
import TcPluginM
import TcRnTypes
import TcType (TcPredType)
import TyCon (TyCon, tyConName)
import Type (Type, splitTyConApp_maybe, isTyVarTy)


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
  , jdiFail    :: Class
  , jdiSucceed :: Class
  }


lookupJDITyCon :: TcPluginM JDI
lookupJDITyCon = do
    Found _ md   <- findImportedModule jdiModule Nothing
    jdiTcNm <- lookupOrig md (mkTcOcc "JustDoIt")
    failTcNm <- lookupOrig md (mkTcOcc "AlwaysFail")
    succeedTcNm <- lookupOrig md (mkTcOcc "Succeed")
    JDI
        <$> tcLookupClass jdiTcNm
        <*> tcLookupClass failTcNm
        <*> tcLookupClass succeedTcNm
  where
    jdiModule  = mkModuleName "GHC.JustDoIt"


getJDI :: Class -> Ct -> Maybe (Ct, TcPredType, [Type])
getJDI c ct =
  getJDI' ct c $ ctev_pred $ cc_ev ct

getJDI' :: Ct -> Class -> Type -> Maybe (Ct, Type, [Type])
getJDI' ct c pred =
  case splitTyConApp_maybe pred of
    Just (x, preds) ->
      case x == classTyCon c of
        True -> Just (ct, pred, preds)
        False -> Nothing
    _ -> Nothing

getLoc :: Ct -> CtLoc
getLoc = ctev_loc . cc_ev


solveJDI :: JDI
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI jdi [] [] allWs = do
  let ws = mapMaybe (getJDI (jdiJDI jdi)) $ allWs
  case length ws of
    0 -> pure $ TcPluginOk [] []
    _ -> do
      z <- traverse (discharge jdi) ws
      pure $ TcPluginOk z []
solveJDI _ _ _ _ = pure $ TcPluginOk [] []



discharge
    :: JDI
    -> (Ct, TcPredType, [Type])
    -> TcPluginM (EvTerm, Ct)
discharge jdi (ct, _, ts) = do
  let [wantedDict] = ts
      loc = getLoc ct

  (className, classParams) <-
    case splitTyConApp_maybe wantedDict of
      Just a  -> pure a
      Nothing -> throw $ PprProgramError "" $ helpMe2 loc


  myclass <- tcLookupClass (tyConName className)
  envs <- getInstEnvs
  case lookupUniqueInstEnv envs myclass classParams of
    -- success!
    Right (clsInst, _) -> do
      let dfun = is_dfun clsInst
      case lookupUniqueInstEnv envs (jdiSucceed jdi) ts of
        Right (successInst, _) ->
          pure
            (EvDFunApp (is_dfun successInst) ts [EvDFunApp dfun [] []], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for Success" err

    -- couldn't find the instance
    Left _ -> do
      when (any isTyVarTy classParams) $ do
        throw $ PprProgramError "" $ helpMe className classParams loc

      case lookupUniqueInstEnv envs (jdiFail jdi) [] of
        Right (clsInst, _) ->
          pure (EvDFunApp (is_dfun clsInst) [] [], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for AlwaysFail" err


helpMe :: TyCon -> [Type] -> CtLoc -> SDoc
helpMe c ts loc = foldl ($$) empty
  [ ppr (tcl_loc $ ctl_env loc)
  , hang empty 2 $ (char '•') <+>
    (
      hang empty 2 $ text "Polymorphic type variables bound in the implicit constraint of 'JustDoIt'" $$ hang empty 2 (ppr (ctl_origin loc))
    )
  , hang empty 2 $ (char '•') <+> text "Probable fix: add an explicit 'JustDoIt ("
      <> ppr c
      <+> foldl (<+>) empty (fmap ppr $ ts )
      <> text ")' constraint to the type signature"
  ]


helpMe2 :: CtLoc -> SDoc
helpMe2 loc = foldl ($$) empty
  [ ppr (tcl_loc $ ctl_env loc)
  , hang empty 2 $ (char '•') <+>
    (
      hang empty 2 $ text "Polymorphic constraint bound in the implicit constraint of 'JustDoIt'" $$ hang empty 2 (ppr (ctl_origin loc))
    )
  , hang empty 2 $ (char '•') <+> text "Probable fix: add an explicit 'JustDoIt c'"
      <+> text "constraint to the type signature"
  ]
