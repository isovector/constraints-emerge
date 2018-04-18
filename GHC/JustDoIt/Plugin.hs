{-# LANGUAGE CPP, TupleSections #-}

module GHC.JustDoIt.Plugin ( plugin ) where


-- external
import           Control.Exception (throw)
import           Control.Monad
import           Data.IORef
import           Data.List (partition)
import           Data.Maybe
import Data.Foldable (for_)

-- GHC API
import VarEnv
import TcType (TcPredType)
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
import GHC (GhcException (..))

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
getLoc c = ctev_loc $ cc_ev c

isJDI :: Class -> Ct -> Bool
isJDI c = isJust . getJDI c


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
        Right (clsInst, _) ->
          pure
            (EvDFunApp (is_dfun clsInst) ts [EvDFunApp dfun [] []], ct)
        Left err ->
          pprPanic "fuck2" $ text "couldn't suceed??"

    -- failure
    Left err -> do
      when (any isTyVarTy classParams) $ do
        throw $ PprProgramError "" $ helpMe className classParams loc

      case lookupUniqueInstEnv envs (jdiFail jdi) [] of
        Right (clsInst, _) ->
          pure (EvDFunApp (is_dfun clsInst) [] [], ct)
        Left err -> pprPanic "fuck" $ text "couldn't fail!"


-- -- did not solve it
-- solveJDI (JDI j _ fail _) _ [a, b] [z]
--   | Just (_, [t])  <- getJDI j z
--   , Just (c, ts) <- splitTyConApp_maybe t
--   =
--   case any isTyVarTy ts of
--     True ->
--       let (dest, loc) = getLoc z
--        in throw $ PprProgramError "" $ helpMe c ts loc
--     False -> do
--       envs <- getInstEnvs
--       case lookupUniqueInstEnv envs fail [] of
--         Right (clsInst, _) ->
--           pure $ TcPluginOk
--             [ (EvLit $ EvNum 0, a)
--             , (EvLit $ EvNum 0, b)
--             , (EvDFunApp (is_dfun clsInst) [] [], z)
--             ]
--             []
--         Left err -> pprPanic "fuck" $ ppr $ ie_global envs


-- -- start
-- solveJDI (JDI jdi w _) _ [] [ws]
--   | Just (pred, [c]) <- getJDI jdi ws
--   = let (dest, loc) = getLoc ws
--      in pure $ TcPluginOk [] [ CNonCanonical $ CtDerived c loc
--                              , CNonCanonical $ CtDerived pred loc
--                              , CNonCanonical $ CtDerived (mkClassPred w []) loc
--                              , ws
--                              ]

-- -- solved it
-- solveJDI (JDI j _ succ) _ [ws] [z]
--   | Just _ <- getJDI w ws
--   , Just (a, [b]) <- getJDI j z
--   = do
--     let Just ok = splitTyConApp_maybe b
--     myclass <- tcLookupClass (tyConName $ fst ok)
--     envs <- getInstEnvs
--     classdfun <- case lookupUniqueInstEnv envs myclass $ snd ok of
--       Right (clsInst, _) -> pure $ is_dfun clsInst
--         -- pprPanic "great" $ ppr clsInst
--         -- pure $ TcPluginOk
--         --   [ (EvLit $ EvNum 0, ws)
--         --   , (EvDFunApp (is_dfun clsInst) [b] [], z)
--         --   ]
--         --   []
--       Left err -> pprPanic "fuck1" $ ppr succ

--     case lookupUniqueInstEnv envs succ [b] of
--       Right (clsInst, _) ->
--         pure $ TcPluginOk
--           [ (EvLit $ EvNum 0, ws)
--           , (EvDFunApp (is_dfun clsInst) [b] [EvDFunApp classdfun [] []], z)
--           ]
--           []
--       Left err -> pprPanic "fuck2" $ ppr succ

-- -- -- recursive case
-- -- solveJDI jdi@(JDI j _ _ _) _ n@[a, b] [z]
-- --   | any (isJDI j) [z]
-- --   , any (isJDI j) n
-- --   = let (m, n') = partition (isJDI j) n
-- --      in solveJDI jdi [] m (z : n')

-- -- solveJDI (JDI j _ fail _) _ [a, b] [z]
-- --   | Just (p, [t])  <- getJDI j z
-- --   , Just (_, ts) <- splitTyConApp_maybe t
-- --   , any isTyVarTy ts
-- --   = pure $ TcPluginOk
-- --           [ (EvLit $ EvNum 0, a)
-- --           , (EvLit $ EvNum 0, b)
-- --           ]
-- --           [ let (dest, loc) = getLoc z
-- --              in CNonCanonical $ CtGiven p dest loc
-- --           ]

-- -- lookupUniqueInstEnv :: InstEnvs -> Class -> [Type] -> Either MsgDoc (ClsInst, [Type])

-- -- solveJDI jdiCls [] [z] [ws]
-- --   | Just c <- getJDI jdiCls ws
-- --   = pprPanic "solved it" $ ppr z


-- -- solveJDI _ a [] c = pprPanic "lool" $ empty
-- solveJDI _ a b c = pure $ TcPluginOk [] []


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
