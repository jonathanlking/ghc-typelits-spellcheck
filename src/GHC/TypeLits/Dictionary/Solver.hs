{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module GHC.TypeLits.Dictionary.Solver
  ( plugin )
where

-- external
import Control.Arrow       ((***))
import Data.List           (partition)
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (lookupModule, lookupName, tracePlugin)
import Language.Aspell     (spellChecker, check, SpellChecker)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm(EvDFunApp))
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginIO)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred, ctEvId)
import TyCon      (TyCon)
import Type       (PredTree (ClassPred),
                   classifyPredType, isStrLitTy)
import Class      (classTyCon)

import FastString (fastStringToByteString)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \_ -> Just $ tracePlugin "Spellcheck" spellcheckPlugin }

spellcheckPlugin :: TcPlugin
spellcheckPlugin =
  TcPlugin { tcPluginInit  = lookupSpellTyCon 
           , tcPluginSolve = solveSpell
           , tcPluginStop  = \_ -> return ()
           }

lookupSpellTyCon :: TcPluginM TyCon
lookupSpellTyCon = do
    md      <- lookupModule sMod sPack
    spellTcNm <- lookupName md (mkTcOcc "Spellcheck")
    tcLookupTyCon spellTcNm
  where
    sMod  = mkModuleName "GHC.TypeLits.Dictionary"
    sPack = fsLit "ghc-typelits-spellcheck"

solveSpell :: TyCon
         -> [Ct] -- ^ [G]iven constraints
         -> [Ct] -- ^ [D]erived constraints
         -> [Ct] -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveSpell _     _ _ []      = return (TcPluginOk [] [])
solveSpell spellTc _ _ wanteds = do
    Right dict <- tcPluginIO spellChecker
    let spellWanteds = mapMaybe (f spellTc dict) wanteds
    let (solved,failed) = (map fst *** map fst) $ partition snd spellWanteds
--    tcPluginIO $ print $ (length solved, length failed)
--    case failed of
--      [] -> return $! TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
--      f  -> return $! TcPluginContradiction f
    -- This is a weird quirk that I'm not sure how to deal with!
    -- I think we want to provide contradictions as early as possible, but then we don't
    -- get an opportunity to provide the Ok solutions, as type checking will abort.
    -- This means that we get 'No instance for ...' errors even for valid words when there is
    -- > 0 misspelt words, which makes debugging 'impossible'.
    case solved of
      [] -> return $! TcPluginContradiction failed
      _  -> return $! TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []

-- Provide bogus dictionary
evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    ClassPred _ _      -> Just $ EvDFunApp (ctEvId ct) [] []
    _                  -> Nothing

f :: TyCon -> SpellChecker -> Ct -> Maybe (Ct, Bool)
f spellTc dict ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    ClassPred c [t] ->
      if (classTyCon c) == spellTc then (\s -> (ct, checkSpell s)) <$> (isStrLitTy t) else Nothing
    _ -> Nothing
  where
    checkSpell s = check dict $ fastStringToByteString s 
