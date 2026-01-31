module Learner.RGL(RGL(..), readGrammar, loadGrammar, writeGrammar) where

import Learner.Config
import System.FilePath
import System.Directory
import GF.Infra.Ident
import GF.Infra.Option
import GF.Grammar.Parser
import GF.Grammar.Printer
import GF.Grammar.Grammar
import GF.Compile
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

data RGL
  = RGL { rglRes       :: ModuleInfo
        , rglCat       :: ModuleInfo
        , rglDict      :: ModuleInfo
        , rglDictAbs   :: ModuleInfo
        , rglParadigms :: ModuleInfo
        }

readGrammar :: Config -> IO RGL
readGrammar cfg = withStatus ("Reading grammar from "++fdir) $ do
  let resX       = moduleNameS (cfgLangModule cfg "Res")
      catX       = moduleNameS (cfgLangModule cfg "Cat")
      cat        = moduleNameS "Cat"
      dictX      = moduleNameS (cfgLangModule cfg "Dict")
      dictXAbs   = moduleNameS (cfgLangModule cfg "Dict"++"Abs")
      paradigmsX = moduleNameS (cfgLangModule cfg "Paradigms")
      predef     = moduleNameS "Predef"

  resM       <- loadModule resX MTResource [] []
  catM       <- loadModule catX (MTConcrete cat) [] [OSimple resX]
  dictM      <- loadModule dictX (MTConcrete dictXAbs) [(catX,MIAll)] [OSimple predef,OSimple resX]
  dictAbsM   <- loadModule dictXAbs MTAbstract [(cat,MIAll)] []
  paradigmsM <- loadModule paradigmsX MTResource [] []
  return (RGL { rglRes = resM{jments=Map.filter notResValue (jments resM)}
              , rglCat = catM
              , rglDict = dictM
              , rglDictAbs = dictAbsM
              , rglParadigms = paradigmsM
              })
  where
    fdir = "src" </> cfgLangName cfg

    loadModule mn@(MN id) mtype mextend mopens = do
      let fpath = fdir </> showIdent id <.> "gf"
      exist <- doesFileExist fpath
      if exist
        then do bs <- BS.readFile fpath
                case runP pModDef bs of
                  Left (Pn l c,msg) -> fail (fpath++":"++show l++":"++show c++":\n   "++msg)
                  Right mi          -> return (snd mi)
        else return (ModInfo {
                         mtype = mtype,
                         mstatus = MSComplete,
                         mflags = noOptions,
                         mextend = mextend,
                         mwith = Nothing,
                         mopens = mopens,
                         mexdeps = [],
                         msrc = fpath,
                         jments = Map.empty
                     })

    notResValue (ResValue _ _) = False
    notResValue _              = True

loadGrammar :: FilePath -> IO (ModuleName,SourceGrammar)
loadGrammar fpath = batchCompile noOptions Nothing [fpath]

writeGrammar :: Config -> RGL -> IO ()
writeGrammar cfg rgl = withStatus ("Writing grammar to "++fdir) $ do
  writeModule (cfgLangModule cfg "Res") (rglRes rgl)
  writeModule (cfgLangModule cfg "Cat") (rglCat rgl)
  writeModule (cfgLangModule cfg "Dict") (rglDict rgl)
  writeModule (cfgLangModule cfg "Dict"++"Abs") (rglDictAbs rgl)
  writeModule (cfgLangModule cfg "Paradigms") (rglParadigms rgl)
  where
    fdir = "src" </> cfgLangName cfg

    writeModule mn mi = do
      let fpath = fdir </> mn <.> "gf"
      writeFile fpath (show (ppModule Unqualified (moduleNameS mn,mi)))
