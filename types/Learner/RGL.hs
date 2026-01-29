module Learner.RGL(RGL(..), readGrammar, writeGrammar) where

import Learner.Config
import Data.Char
import System.FilePath
import System.Directory
import GF.Infra.Ident
import GF.Infra.Option
import GF.Grammar.Parser
import GF.Grammar.Printer
import GF.Grammar.Grammar
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
readGrammar cfg = withStatus ("Writing grammar to "++fdir) $ do
  let resX       = moduleNameS (lang_module "Res" cfg)
      catX       = moduleNameS (lang_module "Cat" cfg)
      cat        = moduleNameS "Cat"
      dictX      = moduleNameS (lang_module "Dict" cfg)
      dictXAbs   = moduleNameS (lang_module "Dict" cfg++"Abs")
      paradigmsX = moduleNameS (lang_module "Paradigms" cfg)
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

writeGrammar :: Config -> RGL -> IO ()
writeGrammar cfg rgl = withStatus ("Writing grammar to "++fdir) $ do
  writeModule (lang_module "Res" cfg) (rglRes rgl)
  writeModule (lang_module "Cat" cfg) (rglCat rgl)
  writeModule (lang_module "Dict" cfg) (rglDict rgl)
  writeModule (lang_module "Dict" cfg++"Abs") (rglDictAbs rgl)
  writeModule (lang_module "Paradigms" cfg) (rglParadigms rgl)
  where
    fdir = "src" </> cfgLangName cfg

    writeModule mn mi = do
      let fpath = fdir </> mn <.> "gf"
      writeFile fpath (show (ppModule Unqualified (moduleNameS mn,mi)))

lang_module pref cfg =
  case cfgIso3 cfg of
    []     -> pref
    (c:cs) -> pref++toUpper c:cs
