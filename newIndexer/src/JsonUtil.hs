{-# LANGUAGE OverloadedStrings     #-}

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

import           Control.Monad
import           Control.Exception          (bracket)
import           Data.Aeson                 (ToJSON, encode)
import qualified Data.Aeson                 as A (ToJSON, toJSON, object, Value(..))
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid                (Monoid(..))
import           System.IO

import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Format           (formatTime)
import           System.Locale              (defaultTimeLocale)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory)

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO a) -> c -> IO a
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
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
