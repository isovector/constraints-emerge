{-# OPTIONS_GHC -Wall #-}

module Data.Constraint.Emerge.Plugin (plugin) where

import           Control.Exception (throw)
import           Control.Monad
import           Data.Bifunctor (second)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable (for)
import           Data.Tuple (swap)
import           Prelude hiding (pred)

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
import Type (Type, TyVar, splitTyConApp_maybe, isTyVarTy, splitAppTy_maybe, getTyVar_maybe, splitAppTys)


------------------------------------------------------------------------------
-- | Match an instance head against a concrete type; returning a substitution
-- from one to the other. If this returns 'mempty', there were no type
-- variables to match.
match
    :: PredType  -- class inst
    -> PredType  -- concrete type
    -> Map TyVar Type
match instClass concClass =
  let Just (_, instHead) = splitAppTy_maybe instClass
      Just (_, concHead) = splitAppTy_maybe concClass
      (_, instTys) = splitAppTys instHead
      (_, concTys) = splitAppTys concHead
   in M.fromList . mapMaybe (fmap swap . sequence . second getTyVar_maybe)
                 $ zip concTys instTys


------------------------------------------------------------------------------
-- | Determine if a 'PredType' is fully monomorphic (ie. has no type
-- variables.)
isMonomorphicCtx
    :: PredType
    -> Bool
isMonomorphicCtx t = isJust $ do
  (_, instHead) <- splitAppTy_maybe t
  let (_, instTys) = splitAppTys instHead
  case null $ mapMaybe getTyVar_maybe instTys of
    True  -> Just ()
    False -> Nothing


------------------------------------------------------------------------------
-- | Substitute all type variables in a 'Type'.
-- TODO(sandy): this should probably operate over a 'PredType' and explicily
-- split and fold, rather than assuming a shape of 'C a b c'.
instantiateHead
    :: Map TyVar Type
    -> Type
    -> Type
instantiateHead mmap t =
  let (tc, tys) = splitAppTys t
      tys' = fmap (\ty -> maybe ty (mmap M.!) $ getTyVar_maybe ty) tys
   in mkAppTys tc tys'


------------------------------------------------------------------------------
-- | Construct an 'EvTerm' witnessing an instance of 'wantedDict' by
-- recursively finding instances, and building dicts for their contexts.
buildDict
    :: CtLoc
    -> PredType
    -> TcPluginM (Maybe EvTerm)
buildDict loc wantedDict = do
  (className, classParams) <-
    case splitTyConApp_maybe wantedDict of
      Just a  -> pure a
      Nothing -> errMissingSig loc

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

           for (sequence mayDicts) $ \dicts ->
             pure $ EvDFunApp dfun (fmap (mmap M.!) vars) dicts

    Left _ -> do
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
findEmergePred :: Class -> Ct -> Maybe (Ct, [PredType])
findEmergePred c ct = do
  let pred = ctev_pred $ cc_ev ct
  case splitTyConApp_maybe pred of
    Just (x, preds) ->
      case x == classTyCon c of
        True -> Just (ct, preds)
        False -> Nothing
    _ -> Nothing


------------------------------------------------------------------------------
-- | Discharge all wanted 'Emerge' constraints.
solveEmerge
    :: EmergeData
    -> [Ct]  -- ^ [G]iven constraints
    -> [Ct]  -- ^ [D]erived constraints
    -> [Ct]  -- ^ [W]anted constraints
    -> TcPluginM TcPluginResult
solveEmerge emerge _ _ allWs = do
  let ws = mapMaybe (findEmergePred $ emergeEmerge emerge) allWs
  case length ws of
    0 -> pure $ TcPluginOk [] []
    _ -> do
      evs <- for ws $ discharge emerge
      pure $ TcPluginOk evs []


------------------------------------------------------------------------------
-- | Discharge an 'Emerge' constraint.
discharge
    :: EmergeData
    -> (Ct, [PredType])
    -> TcPluginM (EvTerm, Ct)
discharge emerge (ct, ts) = do
  let [wantedDict] = ts
      loc = ctev_loc $ cc_ev ct

  when (not $ isMonomorphicCtx wantedDict) $
    errPolyUsage loc

  (className, classParams) <-
    case splitTyConApp_maybe wantedDict of
      Just a  -> pure a
      Nothing -> errMissingSig loc

  envs      <- getInstEnvs
  mayMyDict <- buildDict loc wantedDict

  case mayMyDict of
    -- we successfully built a dict
    Just myDict ->
      case lookupUniqueInstEnv envs (emergeSucceed emerge) ts of
        Right (successInst, _) ->
          pure (EvDFunApp (is_dfun successInst) ts [myDict], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for Success" err

    -- couldn't find the instance
    Nothing -> do
      when (any isTyVarTy classParams) $
        errPolyClassTys className classParams loc

      case lookupUniqueInstEnv envs (emergeFail emerge) [] of
        Right (clsInst, _) ->
          pure (EvDFunApp (is_dfun clsInst) [] [], ct)
        Left err ->
          pprPanic "couldn't get a unique instance for AlwaysFail" err


------------------------------------------------------------------------------
-- | Error out complaining about polymorphic type variables in the implicit
-- context.
errPolyClassTys :: TyCon -> [Type] -> CtLoc -> a
errPolyClassTys c ts loc = errHelper loc
  [ text "Polymorphic type variables bound in the implicit constraint of 'Emerge'"
      $$ hang empty 2 (ppr (ctl_origin loc))
  , text "Probable fix: add an explicit 'Emerge ("
      <> ppr c
      <+> foldl (<+>) empty (fmap ppr $ ts )
      <> text ")' constraint to the type signature"
  ]


------------------------------------------------------------------------------
-- | Erorr out complaining about a polymorphic constraint.
errMissingSig :: CtLoc -> a
errMissingSig loc = errHelper loc
  [ text "Polymorphic constraint bound in the implicit constraint of 'Emerge'"
      $$ hang empty 2 (ppr (ctl_origin loc))
  , text "Probable fix: add an explicit 'Emerge c'"
      <+> text "constraint to the type signature"
  ]


------------------------------------------------------------------------------
-- | Error out complaining about a polymorphic usage.
errPolyUsage :: CtLoc -> a
errPolyUsage loc = errHelper loc
  [ text "Polymorphic usage of function bound with an implicit 'Emerge' constraint"
      $$ hang empty 2 (ppr (ctl_origin loc))
  , text "'Emerge' can only be discharged when it is fully monomorphic"
  , text "Fix: make the call-site monomorphic, or add an explicit 'Emerge c'"
      $$ hang empty 2 (text "constraint to the type-signature.")
  ]


------------------------------------------------------------------------------
-- | Helper function to generate pretty error messages.
errHelper :: CtLoc -> [SDoc] -> a
errHelper loc ss
  = throw
  . PprProgramError ""
  . hang (ppr . tcl_loc $ ctl_env loc) 2
  . foldl ($$) empty
  $ fmap (\z -> hang empty 0 $ char '•' <+> z) ss

