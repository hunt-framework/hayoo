{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Hunt.NewPkgIndexerCore
where

import Debug.Trace
import Text.Show.Pretty (ppShow)

import           Control.Applicative          ((<$>))
import           Control.DeepSeq              (NFData, rnf)

import           Data.Binary                  (Binary)
import qualified Data.Binary                  as B
import qualified Data.StringMap.Strict        as M
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)

import           Hayoo.Hunt.ApiDocument       (PkgDescr(PD), RankDescr, boringApiDoc, toApiDoc)

import           Hayoo.Hunt.IndexSchema       (appendSaveCmd,
                                               c'type, c'indexed, c'name, c'partial, c'upload,
                                               c'author, c'category, c'synopsis, c'description,
                                               c'dependencies,
                                               d'indexed, d'package,
                                               fmtDateXmlSchema, fmtDateHTTP, parseDateHTTP)
import           Hayoo.PackageInfo            (PackageInfo(p_uploaddate, p_rank))

import           Hayoo.PackageInfo            (PackageInfo(p_uploaddate, p_rank))

import           Holumbus.Crawler             (URI)

import           Holumbus.Crawler.IndexerCore (IndexerState(..),
                                               IndexCrawlerConfig, IndexCrawlerState,
                                               IndexContextConfig,
                                               RawDoc,
                                               emptyIndexerState, indexCrawlerConfig')

import           Hunt.ClientInterface          hiding (URI)

import           Text.XML.HXT.Core             (IOSArrow, SysConfig, XmlTree, (***))

import qualified Hunt.Common.ApiDocument       as H
import qualified Data.Text                     as Text
import           Data.Char
import qualified Data.Map.Lazy                 as LazyMap
import           Hayoo.PackageInfo

buildDocIndex :: UTCTime -> String -> PackageInfo -> H.IndexMap
buildDocIndex now pkgName pkgInfo = LazyMap.fromList $ 
     add c'author       (parseWords $ p_author pkgInfo)
  ++ add c'category     (parseWords $ p_category pkgInfo)
  ++ add c'dependencies (parseWords $ p_dependencies pkgInfo)
  ++ add c'description  (parseWords $ p_description pkgInfo)
  ++ add c'synopsis     (parseWords $ p_synopsis pkgInfo)
  ++ add c'name         [Text.pack pkgName]
  ++ add c'type         ["package"]
  ++ add c'indexed      [ now' ]
  ++ add c'upload       [ upl ]
  ++ add c'partial      [ ns ]
  where
    add keyword words =  [ (keyword, Text.unwords words) ]
    parseWords str = filter (not . Text.null) $ Text.split (not . isLetter) (Text.pack str)
    now'  = fmtDateXmlSchema now
    -- now'' = fmtDateHTTP      now

    upl = maybe "" id uplDate
        where
          uplDate
              = do let dt1 = p_uploaddate pkgInfo
                   pd  <- parseDateHTTP dt1
                   return $ fmtDateXmlSchema pd

    names = T.words . Text.pack $ pkgName
    (n, ns) = (T.concat *** T.concat) . splitAt 1 $ names

toCommand :: Bool -> UTCTime -> Bool -> [(String, Maybe PackageInfo)] -> Command
toCommand save now update pkgs
      = deletePkgCmd
{-
        appendSaveCmd save now $
        cmdSequence [ deletePkgCmd
                    , cmdSequence . concatMap toCmd $ pkgs
                    ]
-}
    where
      deletePkgCmd
          | update && not (null pkgs)
              = cmdDeleteDocsByQuery
                . qAnd ( setContext c'type
                         $ qFullWord d'package
                     )
                . setContext c'name
                . qOrs
                . map (\(t,_) -> qFullWord . T.pack $ t)
                $ pkgs

          | otherwise
              = cmdNOOP

