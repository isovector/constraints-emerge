{-# OPTIONS_GHC -Wall #-}

module Data.Constraint.Emerge.Plugin (plugin) where

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
import TyCon (TyCon, tyConName)
import Type (Type, splitTyConApp_maybe, isTyVarTy)


------------------------------------------------------------------------------
-- | The exported 'Emerge' plugin.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just emergePlugin }


------------------------------------------------------------------------------
-- | Actual implementation of the plugin.
emergePlugin :: TcPlugin
emergePlugin = TcPlugin
  { tcPluginInit  = lookupEmergeTyCons
  , tcPluginSolve = solveEmerge
  , tcPluginStop  = const (return ())
  }


------------------------------------------------------------------------------
-- | Class instances from 'Data.Constraint.Emerge' necessary for us to know
-- about.
data EmergeData = EmergeData
  { emergeEmerge  :: Class
  , emergeFail    :: Class
  , emergeSucceed :: Class
  }


------------------------------------------------------------------------------
-- | Lookup the classes from 'Data.Constraint.Emerge' and build an
-- 'EmergeData'.
lookupEmergeTyCons :: TcPluginM EmergeData
lookupEmergeTyCons = do
    Found _ md  <- findImportedModule emergeModule Nothing
    emergeTcNm  <- lookupOrig md $ mkTcOcc "Emerge"
    failTcNm    <- lookupOrig md $ mkTcOcc "AlwaysFail"
    succeedTcNm <- lookupOrig md $ mkTcOcc "Succeed"

    EmergeData
        <$> tcLookupClass emergeTcNm
        <*> tcLookupClass failTcNm
        <*> tcLookupClass succeedTcNm
  where
    emergeModule  = mkModuleName "Data.Constraint.Emerge"


------------------------------------------------------------------------------
-- | Determines if 'ct' is an instance of the 'c' class, and if so, splits out
-- its applied types.
findEmergePred :: Class -> Ct -> Maybe (Ct, [Type])
findEmergePred c ct = do
  let pred = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe pred of
    Just (x, preds) ->
      case x == classTyCon c of
        True -> Just (ct, preds)
        False -> Nothing
    _ -> Nothing


------------------------------------------------------------------------------
-- | Get the original source location a 'Ct' came from. Used to generate error
-- messages.
getLoc :: Ct -> CtLoc
getLoc = ctev_loc . cc_ev


------------------------------------------------------------------------------
-- | Discharge all wanted 'Emerge' constraints.
solveEmerge
    :: EmergeData
    -> [Ct]  -- ^ [G]iven constraints
    -> [Ct]  -- ^ [D]erived constraints
    -> [Ct]  -- ^ [W]anted constraints
    -> TcPluginM TcPluginResult
solveEmerge emerge _ _ allWs = do
  let ws = mapMaybe (findEmergePred (emergeEmerge emerge)) $ allWs
  case length ws of
    0 -> pure $ TcPluginOk [] []
    _ -> do
      z <- traverse (discharge emerge) ws
      pure $ TcPluginOk z []


------------------------------------------------------------------------------
-- | Discharge an 'Emerge' constraint.
discharge
    :: EmergeData
    -> (Ct, [Type])
    -> TcPluginM (EvTerm, Ct)
discharge emerge (ct, ts) = do
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
      case lookupUniqueInstEnv envs (emergeSucceed emerge) ts of
        Right (successInst, _) -> pure
            (EvDFunApp (is_dfun successInst) ts [EvDFunApp dfun [] []], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for Success" err

    -- couldn't find the instance
    Left _ -> do
      when (any isTyVarTy classParams) $ do
        throw $ PprProgramError "" $ helpMe className classParams loc

      case lookupUniqueInstEnv envs (emergeFail emerge) [] of
        Right (clsInst, _) ->
          pure (EvDFunApp (is_dfun clsInst) [] [], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for AlwaysFail" err


helpMe :: TyCon -> [Type] -> CtLoc -> SDoc
helpMe c ts loc = foldl ($$) empty
  [ ppr (tcl_loc $ ctl_env loc)
  , hang empty 2 $ (char '•') <+>
    (
      hang empty 2 $ text "Polymorphic type variables bound in the implicit constraint of 'Emerge'"
                  $$ hang empty 2 (ppr (ctl_origin loc))
    )
  , hang empty 2 $ (char '•') <+> text "Probable fix: add an explicit 'Emerge ("
      <> ppr c
      <+> foldl (<+>) empty (fmap ppr $ ts )
      <> text ")' constraint to the type signature"
  ]


helpMe2 :: CtLoc -> SDoc
helpMe2 loc = foldl ($$) empty
  [ ppr (tcl_loc $ ctl_env loc)
  , hang empty 2 $ (char '•') <+>
    (
      hang empty 2 $ text "Polymorphic constraint bound in the implicit constraint of 'Emerge'"
                  $$ hang empty 2 (ppr (ctl_origin loc))
    )
  , hang empty 2 $ (char '•') <+> text "Probable fix: add an explicit 'Emerge c'"
      <+> text "constraint to the type signature"
  ]

