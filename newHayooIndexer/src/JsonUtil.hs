{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonUtil
  ( jsonOutput
  , jsonPutStr
  , hJsonPutStr
  , outputValue
  , emitJsonList
  , UTCTime
  , fmtDateXmlSchema
  , fmtDateHTTP
  , pair
  , APair
  , A.toJSON
  , A.Value(..)
  , A.object
  , buildNOOP
  , buildUpdateWeight
  , fullWord
  )
where

import           Control.Exception          (bracket)
import           Control.Monad
import           Data.Aeson                 (ToJSON, encode)
import qualified Data.Aeson                 as A (ToJSON, Value (..), object,
                                                  toJSON)
import           Data.Aeson.Encode.Pretty   (Config (..), Indent (..),
                                             encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid                (Monoid (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory)
import           System.IO

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO a) -> c -> IO a
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
#if MIN_VERSION_aeson_pretty(0, 8, 0)
        indent = Spaces 2
#else
        indent = 2
#endif

        encConfig :: Config
        encConfig
            = Config { confIndent = indent
                     , confCompare
                         = keyOrder ["description", "index", "uri"]
                           `mappend`
                           compare
                     }

jsonPutStr :: (ToJSON c) => Bool -> c -> IO ()
jsonPutStr pretty c = jsonOutput pretty LC.putStrLn c

hJsonPutStr pretty h c = jsonOutput pretty (LC.hPutStrLn h) c

-- Output a JSON value to a file. Create the parent directory if necessary.
outputValue :: ToJSON c => String -> c -> IO ()
outputValue path c = do
  let dirPath = takeDirectory path
  createDirectoryIfMissing True dirPath
  bracket (openBinaryFile path WriteMode) hClose (\h -> hJsonPutStr True h c)

-- | Emit a JSON list.
emitJsonList :: Handle -> [ A.Value ] -> IO ()
emitJsonList h []  = hPutStrLn h "[]"
emitJsonList h [x] = do hPutStrLn h "["
                        hJsonPutStr True h x
                        hPutStrLn h "]"
emitJsonList h (x:xs) = do
  hPutStrLn h "["
  hJsonPutStr True h x
  forM_ xs $ \y -> do
    hPutStrLn h ", "
    hJsonPutStr True h y
  hPutStrLn h "]"

--- Command building utilities

fmtDateXmlSchema :: UTCTime -> String
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> String
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> String
fmtDate' fmt
    = formatTime defaultTimeLocale fmt

pair :: A.ToJSON c => String -> c -> (Text, A.Value)
pair k v = (T.pack k, A.toJSON v)

type APair = (Text, A.Value)

-- Build a case-fullword clause for a query.
fullWord s = A.object [ pair "op"   ("case" :: String)
                      , pair "type" ("fullword" :: String)
                      , pair "word" s
                      ]

-- Build a NOOP command.
buildNOOP :: A.Value
buildNOOP = A.object [ pair "cmd" ("noop" :: String) ]

-- Build an update weight command.
buildUpdateWeight uri weight =
  A.object [ pair "cmd"      ("update" :: String)
           , pair "document" (A.object [ pair "uri"    uri
                                       , pair "weight" weight
                                       ]
                             )
          ]
