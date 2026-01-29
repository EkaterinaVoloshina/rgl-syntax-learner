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
  res       <- loadModule (lang_module "Res" cfg) MTResource []
  cat       <- loadModule (lang_module "Cat" cfg) (MTConcrete (moduleNameS "Cat")) [OSimple (moduleNameS (lang_module "Res" cfg))]
  dict      <- loadModule (lang_module "Dict" cfg) (MTConcrete (moduleNameS (lang_module "Dict" cfg++"Abs"))) []
  dictAbs   <- loadModule (lang_module "Dict" cfg++"Abs") MTAbstract []
  paradigms <- loadModule (lang_module "Paradigms" cfg) MTResource []
  return (RGL { rglRes = res
              , rglCat = cat
              , rglDict = dict
              , rglDictAbs = dictAbs
              , rglParadigms = paradigms
              })
  where
    fdir = "src" </> cfgLangName cfg

    loadModule mn mtype mopens = do
      let fpath = fdir </> mn <.> "gf"
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
                         mextend = [],
                         mwith = Nothing,
                         mopens = mopens,
                         mexdeps = [],
                         msrc = fpath,
                         jments = Map.empty
                     })

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
