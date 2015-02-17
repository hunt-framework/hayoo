module Hayoo.Index.Hoogle (
    Fact
  , Inst
  , MkURI
  , mkHaddockUri
  , indexHoogleArchive
  ) where

import           Conduit
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Data.Function (on)
import qualified Data.List as List
import           Data.Monoid
import           Hayoo.Index.Conduit
import           Hayoo.Index.Hoogle.FunctionInfo
import           Hayoo.Index.Hoogle.Parser1
import           Hunt.ClientInterface
import           Hunt.Conduit
import           Hunt.Server.Client
import           System.FilePath

mkHaddockUri :: String -> MkURI (Inst Fact)
mkHaddockUri base package version module_ decl anchor =
  mconcat [
      base
    , "/package/"
    , package
    , "-"
    , version
    , "/docs/"
    , module'
    , ".html"
    , "#"
    , escape (anchor decl)
    ]
  where
    escape = concatMap (\c -> case c of
                           '\'' -> "-39-"
                           _    -> return c)

    module' = fmap (\c -> case c of
                       '.' -> '-'
                       _   -> c) module_

indexHoogleArchive :: MkURI (Inst Fact) -> FilePath -> HuntConnectionT IO ()
indexHoogleArchive mkUri fp = do
  archive <- liftIO $ ByteString.readFile fp
  compressedArchive archive
    =$= functionInfos mkUri
    =$= leftLogger "hoogle:"
    =$= makeInserts toApiDocument
    =$= rechunkCommands 50
    $$ cmdSink

functionInfos :: Monad m
              => MkURI (Inst Fact)
              ->  Conduit (FilePath, ByteString) m (Either (Line Error) FunctionInfo)
functionInfos mkUri = concatMapC go
  where
    go (fp, content) =
      let
        (package:version:_) = splitDirectories fp
        (insts, errors)     = parseHoogle (utf8 content)
        infos               = functionInfo mkUri package version insts
        dedupe              = List.nubBy ((==) `on` fiURI) infos
        dispError err       = package ++ "-" ++ version ++ ": " ++ err
      in
       fmap (Left . fmap dispError) errors ++ fmap Right dedupe
