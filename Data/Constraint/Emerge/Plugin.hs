{-# OPTIONS_GHC -Wall #-}

module Data.Constraint.Emerge.Plugin (plugin) where

import Data.Bifunctor (first, second)
import Data.Tuple (swap)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Exception (throw)
import Control.Monad
import Data.Maybe
import Prelude hiding (pred)

import UniqSet (isEmptyUniqSet)
import TcType
import Class hiding (className)
import GHC (GhcException (..), idType)
import InstEnv (ClsInst (..), lookupUniqueInstEnv)
import Module (mkModuleName)
import OccName (mkTcOcc)
import Outputable
import Plugins (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (EvDFunApp))
import TcPluginM
import TcRnTypes
import TyCon (TyCon, tyConName)
-- import Type (Type, splitTyConApp_maybe, isTyVarTy, pprSigmaType, splitAppTy_maybe)
import Type


------------------------------------------------------------------------------
-- | match an instance head against a concrete type
match
    :: Type  -- class inst
    -> Type  -- concrete type
    -> Map TyVar Type
match instClass concClass =
  let Just (_, instHead) = splitAppTy_maybe instClass
      Just (_, concHead) = splitAppTy_maybe concClass
      (_, instTys) = splitAppTys instHead
      (_, concTys) = splitAppTys concHead
   in M.fromList . mapMaybe (fmap swap . sequence . second getTyVar_maybe)
                 $ zip concTys instTys


instantiateHead
    :: Map TyVar Type
    -> Type
    -> Type
instantiateHead mmap t =
  let (tc, tys) = splitAppTys t
      tys' = fmap (\ty -> maybe ty (mmap M.!) $ getTyVar_maybe ty) tys
   in mkAppTys tc tys'


------------------------------------------------------------------------------
-- | we can use givens' 'EvVar's as 'EvId' terms
buildDict
    :: CtLoc
    -> Type
    -> TcPluginM (Maybe EvTerm)
buildDict loc wantedDict = do
  (className, classParams) <-
    case splitTyConApp_maybe wantedDict of
      Just a  -> pure a
      Nothing -> throw $ PprProgramError "" $ helpMe2 loc

  myclass <- tcLookupClass $ tyConName className
  envs    <- getInstEnvs

  case lookupUniqueInstEnv envs myclass classParams of
    -- success!
    Right (clsInst, _) -> do
      let dfun = is_dfun clsInst
          (vars, subclasses, inst) = tcSplitSigmaTy $ idType dfun
          mmap = match inst wantedDict

      if null subclasses
         then pure . Just $ EvDFunApp dfun [] []
         else do
           mayDicts <- traverse (buildDict loc . instantiateHead mmap) subclasses
           case sequence mayDicts of
             -- fail if any of our subdicts failed
             Nothing -> pure Nothing
             Just dicts -> pure . Just $ EvDFunApp dfun (fmap (mmap M.!) vars) $ dicts
    Left err -> do
      -- check givens?
      pure Nothing



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

  -- when (not $ isEmptyUniqSet $ allBoundVariables wantedDict) $
  -- pprPanic "bye" $ ppr $ allBoundVariables wantedDict

  mayMyDict <- buildDict loc wantedDict
  case mayMyDict of
    Just myDict ->
      case lookupUniqueInstEnv envs (emergeSucceed emerge) ts of
        Right (successInst, _) -> pure
            -- TODO(sandy): need to type apply classParams
            -- and get dicts for them somehow
            (EvDFunApp (is_dfun successInst) ts [myDict], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for Success" err

    -- couldn't find the instance
    Nothing -> do
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
