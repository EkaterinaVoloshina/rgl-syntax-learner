module Learner.PidginSyntax(options,learn) where

import System.FilePath
import System.Console.GetOpt
import Learner.RGL
import Learner.Config
import Learner.CodeGen
import GF.Grammar
import GF.Grammar.Lookup
import GF.Grammar.Lockfield
import GF.Compile.Compute (normalForm,stdPredef,Globals(..))
import GF.Text.Pretty
import GF.Data.Operations
import GF.Infra.CheckM
import Control.Monad
import qualified Data.Map as Map

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  ]




-- | main function to learn different parts of a grammar
learn cfg = do
  (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Lang" ++".gf")
  let grammar_mn = cfgLangModuleName cfg "Grammar"
  grammar_mo <- lookupModule gr grammar_mn
  grammar_mo <-
    foldM (\grammar_mo (abs_mn,abs_mi) -> do
              let name   = show (pp abs_mn)
                  cnc_mn = cfgLangModuleName cfg name
                  cnc_mi = case lookupModule gr cnc_mn of
                             Ok mi -> mi
                             _     -> getModule cfg abs_mn []
              if elem name ["Adjective","Adverb","Conjunction","Idiom",
                            "Names","Noun","Phrase","Question",
                            "Relative","Sentence","Symbol","Verb"]
                then do putStrLn ("== "++name++" ==")
                        let cnc_mi' = Map.foldlWithKey (addPidginCncFun gr cnc_mn) cnc_mi (jments abs_mi)
                        if Map.size (jments cnc_mi') /= Map.size (jments cnc_mi)
                          then do writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg name ++ ".gf")
                                            (show (ppModule Unqualified (cnc_mn,cnc_mi')))
                                  case lookup cnc_mn (mextend grammar_mo) of
                                    Nothing -> return grammar_mo{mextend=(cnc_mn,MIAll):mextend grammar_mo}
                                    Just _  -> return grammar_mo
                          else return grammar_mo
                else do return grammar_mo)
          grammar_mo
          [mo | mo@(_,mi) <- modules gr, isModAbs mi]
  writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Grammar" ++ ".gf") (show (ppModule Unqualified (grammar_mn,grammar_mo)))
  where
    addPidginCncFun gr mn mi name (AbsFun (Just l_ty) _) =
      case runCheck (linTypeOfType cfg gr mn l_ty) of
        Ok (ty,_) -> case Map.lookup name (jments mi) of
                       Just _  -> mi
                       Nothing -> mi{jments=Map.insert name (pidginCncFun gr ty) (jments mi)}
        Bad msg   -> mi
    addPidginCncFun gr mn mi _ _ = mi

    pidginCncFun gr ty = CncFun Nothing (Just (L NoLoc (generateTerm gr [] ty))) Nothing Nothing

linTypeOfType :: Config -> Grammar -> ModuleName -> L Type -> Check Type
linTypeOfType cfg cnc m (L loc typ) = do
  let (ctxt,res_cat) = typeSkeleton typ
  val <- linCatOf res_cat
  args <- mapM mkLinArg (zip [1..] ctxt)
  return (mkProd args val [])
 where
   mkLinArg (i,(n,cat)) = do
     val  <- linCatOf cat
     let vars = mkRecType varLabel $ replicate n typeStr
     rec <- if n==0 then return val else
                       errIn (render ("extending" $$
                                      nest 2 vars $$
                                      "with" $$
                                      nest 2 val)) $
                             plusRecType vars val
     return (Explicit,varX i,rec)
   linCatOf (MN m,c) = do
     let cnc_m  = cfgLangModuleName cfg "Cat"
     rec <- lookupLincat cnc cnc_m c >>= normalForm g
     return (lock c rec)
   g = Gl cnc (stdPredef g) False
