{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.Internal
  ( fullWord
  , fmtTime
  , (|>)
  ) where


import           Data.Aeson as Json
import qualified Data.Text  as T
import qualified Data.Time  as Time



-- HUNT COMMAND HELPERS


fullWord :: T.Text -> Json.Value
fullWord word =
  Json.object
    [ "op"   .= ("case" :: String)
    , "type" .= ("fullword" :: String)
    , "word" .= word
    ]



-- DATE HELPERS


fmtTime :: String -> Time.UTCTime -> String
fmtTime format =
  Time.formatTime Time.defaultTimeLocale format



-- HELPERS


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
