module Main where

import System.Environment (getArgs)
import Control.Monad
import Control.Exception
import Data.Functor
import Unsafe.Coerce

import GHC
import GHC.Paths (libdir)
import DynFlags

import HVG.Context2D
import HVG.Type

main :: IO ()
main = do
  args <- getArgs

  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    draw <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags $ dflags
        { hscTarget = HscInterpreted
        , ghcLink   = LinkInMemory
        }

      targets <- forM args (\arg -> guessTarget arg Nothing)
      setTargets targets
      load LoadAllTargets
      setContext [IIModule $ mkModuleName "Main"]

      unsafeCoerce <$> compileExpr "main" :: Ghc (IO ())
    draw
--      res <- runStmt "main" RunToCompletion
--      case res of
--        RunException e -> throw e
--        _ -> return ()

  return ()
