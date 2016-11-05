{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hayoo.App
  ( -- * Types
    HayooApp (..)
  , HayooEnv (..)
  , HayooErr (..)

    -- * Operations
  , search
  , autocomplete
  , selectPackageVersion
  ) where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson           (decode, encode)
import qualified Data.Text            as T
import           Data.Time
import           Hayoo.App.Types
import           Hayoo.ParseSignature
import qualified Hunt.Client          as HC
import           Hunt.ClientInterface (qAnd, qAnds, qContext, qFullWord, qOrs,
                                       qPhrase, qWord, qWordNoCase, setBoost,
                                       setContexts)
import qualified Hunt.ClientInterface as HC
import           Servant.Client       (ClientEnv, ClientM, ServantError)
import qualified Text.Parsec          as P


-- TYPES

newtype HayooApp a = HayooApp
  { unHayoo :: ReaderT HayooEnv (ExceptT HayooErr IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HayooEnv, MonadError HayooErr)


data HayooEnv = HayooEnv
  { envClient :: ClientEnv
  }


data HayooErr
  = ParserErr P.ParseError
  | HuntClientErr ServantError
  | InvalidCmdResult HC.CmdResult
  deriving (Show)


-- OPERATIONS

search :: T.Text -> Int -> HayooApp (HC.LimitedResult SearchResult)
search queryText page = runRequest $ HC.search query offset limit
  where
    query  = parseHayooQuery queryText
    offset = Just $ page * 20
    limit  = Just 20


autocomplete :: T.Text -> HayooApp [T.Text]
autocomplete queryText = do
  suggestionsWithScore <- runRequest $ HC.complete query Nothing
  return $ handleResults $ fst <$> suggestionsWithScore
  where
    query = parseHayooQuery queryText

    handleResults :: [T.Text] -> [T.Text]
    handleResults completions
      | isSignatureQuery queryText = completions
      | otherwise = HC.printQuery <$> HC.completeQueries query completions


selectPackageVersion :: T.Text -> HayooApp (Maybe T.Text)
selectPackageVersion packageName = do
  result <- runRequest $ HC.eval $ packageVersionCmd packageName
  case result of
    HC.ResSearch limitedResult -> do
      versionResult <- return $ decode $ encode $ HC.lrResult limitedResult
      return $ versionResult >>= packageVersion

    clientResult ->
      throwError $ InvalidCmdResult clientResult


time :: HayooApp a -> HayooApp (a, Double)
time action = do
  t0 <- liftIO getCurrentTime
  r  <- action
  t1 <- liftIO getCurrentTime
  let delta = t1 `diffUTCTime` t0
  return (r, (realToFrac delta))


-- HUNT COMMANDS

packageVersionCmd :: T.Text -> HC.Command
packageVersionCmd packageName =
  HC.setMaxResults 1 $ HC.cmdSearch $ HC.qAnds constraints
  where
    constraints =
      [ HC.setContext "type" (HC.qFullWord "package")
      , HC.setContext "name" (HC.qFullWord packageName)
      ]


-- QUERIES

parseHayooQuery :: T.Text -> HC.Query
parseHayooQuery q = qOrs $ concat [stdq, sigq, defq]
  where
    sigq :: [HC.Query]
    sigq = map (\ q' -> qOrs $ q' : subSigq) sig1q

    stdq :: [HC.Query]
    stdq
      | isSig     = []
      | otherwise = either (const []) (:[])   -- throw away errors
                    $ HC.parseQuery (T.unpack q)       -- try to parse q as hunt query

    defq :: [HC.Query]
    defq
      | null sigq                          -- if both query parsers fail,
        &&
        null stdq = [ qAnds                -- build a default query (AND)
                      . map qWordNoCase
                      . map removeQuotes
                      $ T.words q       -- from the list of words
                    ]
      | otherwise = []

    isSig = isSignatureQuery q

    sig :: [Signature]
    sig = ( if isSig
            then id
            else complexSignatures 3
          )                            -- throw away too simple queries
          . either (const []) ((:[]))  -- throw away parser errors
          . parseNormSignature         -- try to parse q as signature
          . T.unpack
          $ removeQuotes q

    subSigs :: [Signature]
    subSigs = concatMap (complexSignatures 1 . subSignatures) sig

    subSigq :: [HC.Query]
    subSigq
      | null subSigs = []
      | otherwise    = (:[])
                       . setBoost 0.1    -- results of sub signature queries have a reduced score
                       . setContexts ["subsig"]
                       . qAnds
                       . map ( qFullWord  -- exact case sensitive word search
                               . T.pack       -- sub signatures must be found in sub signature context
                               . prettySignature
                             )
                       $ subSigs

    sig1q :: [HC.Query]
    sig1q = map ( setContexts ["signature"]
                  . qWord             -- case sensitive prefix search
                  . T.pack                -- convert to Text
                  . prettySignature   -- convert Signature into String
                ) sig


-- HELPERS

runRequest :: ClientM a -> HayooApp a
runRequest req = do
  client <- asks envClient
  result <- liftIO $ HC.runClientM req client
  either (throwError . HuntClientErr) return result


packageVersion :: [PackageVersionResult] -> Maybe T.Text
packageVersion (PackageVersionResult v:[]) = Just v
packageVersion _ = Nothing


removeQuotes :: T.Text -> T.Text
removeQuotes t
  | T.null t        = t
  | T.head t == '"'
    &&
    T.last t == '"' = T.dropAround (== '"') t
  | T.head t == '\''
    &&
    T.last t == '\'' = T.dropAround (== '\'') t
  | otherwise        = t


isSignatureQuery :: T.Text -> Bool
isSignatureQuery q
  = "->" `T.isInfixOf` q
    ||
    "=>" `T.isInfixOf` q
