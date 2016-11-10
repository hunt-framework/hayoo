{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hayoo.App
import qualified Hunt.Client          as HC
import qualified Hunt.ClientInterface as HC
import           Servant.Client
import qualified System.Metrics       as EKG

main :: IO ()
main = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  env <- HayooEnv <$> HC.withBaseUrl HC.huntBaseUrl <*> newMetrics store
  result <- runHayoo (measure searches (search "Reisner" 0)) env
  result' <- stats $ searches $ envMetrics $ env
  putStrLn $ show $ result
