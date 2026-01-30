module Learner.Shell(module Learner.Config, learnerMain) where

import Learner.Config
import Learner.CodeGen
import qualified Learner.MorphoCats
import qualified Learner.SyntaxLearner
import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Exception
import GHC.IO.Exception

learnerMain cfg = do
  args <- getArgs
  case args of
    ("morpho-cats":args) -> withConfig cfg args Learner.MorphoCats.options Learner.MorphoCats.learn
    ("syntax":args)      -> withConfig cfg args Learner.SyntaxLearner.options Learner.SyntaxLearner.learn
    _                    -> do hPutStrLn stderr "Synopsis:"
                               hPutStrLn stderr (usageInfo (cfgLangName cfg++" morpho-cats <options>") Learner.MorphoCats.options)
                               hPutStrLn stderr (usageInfo (cfgLangName cfg++" syntax <options>") Learner.SyntaxLearner.options)

withConfig cfg args options f = do
  let (opts,nonopts,errs) = getOpt Permute options args
  case (nonopts,errs) of
    (_:_,_) -> hPutStrLn stderr ("don't know what to do with "++show nonopts)
    (_,_:_) -> mapM_ (hPutStrLn stderr) errs
    _       -> catch (f (foldr id cfg opts))
                     (\err@(IOError{}) ->
                         case ioe_type err of
                           UserError -> hPutStrLn stderr (ioe_description err)
                           _         -> hPutStrLn stderr (show err))
