{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Common

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit as HTTP
import System.Environment

main :: IO ()
main = do
    [keyword] <- getArgs

    twInfo <- getTWInfoFromEnv

    withManager $ \mgr -> do
        src <- stream twInfo mgr $ statusesFilterByTrack $ T.pack keyword
        src C.$$+- CL.mapM_ (liftIO . printTL)

printTL :: StreamingAPI -> IO ()
printTL = print

