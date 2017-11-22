{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hayoo.App
  ( -- * App
    App (..)
  , Env (..)
  , Error (..)
  , Metrics (..)

    -- * Eval
  , run
  , observe

    -- * Operations
  , measure
  , search
  , autocomplete
  , selectPackageVersion

    -- * Metrics
  , Metric
  , newMetrics
  , stats
  , currentCount
  , metrics
  ) where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson           (decode, encode)
import qualified Data.Text            as T
import           Hayoo.App.Error
import           Hayoo.App.Metrics
import           Hayoo.App.Types
import           Hayoo.ParseSignature
import qualified Hunt.Client          as HC
import           Hunt.ClientInterface (qAnd, qAnds, qContext, qFullWord, qOrs,
                                       qPhrase, qWord, qWordNoCase, setBoost,
                                       setContexts)
import qualified Hunt.ClientInterface as HC
import           Servant.Client       (ClientEnv, ClientM, ServantError)
import           System.Metrics.Json  (Sample)



-- APP


newtype App a = App
  { unHayoo :: ReaderT Env (ExceptT Error IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
             , MonadError Error
             )


data Env
  = Env
    { envClient  :: !ClientEnv
    , envMetrics :: !Metrics
    }


data Metrics = Metrics
  { _searches    :: !Metric
  , _completions :: !Metric
  }



-- EVAL


run :: App a -> Env -> IO (Either Error a)
run app env =
  runExceptT (runReaderT (unHayoo app) env)


observe :: App a -> App (Either Error a)
observe app = do
  env <- ask
  liftIO (run app env)



-- OPERATIONS


search :: T.Text -> Int -> App (HC.LimitedResult SearchResult)
search queryText page = runRequest $ HC.search query offset limit
  where
    query  = parseHayooQuery queryText
    offset = Just $ page * 20
    limit  = Just 20


autocomplete :: T.Text -> App [T.Text]
autocomplete queryText = do
  suggestionsWithScore <- runRequest $ HC.complete query Nothing
  return $ handleResults $ fst <$> suggestionsWithScore
  where
    query = parseHayooQuery queryText

    handleResults :: [T.Text] -> [T.Text]
    handleResults completions
      | isSignatureQuery queryText = completions
      | otherwise = HC.printQuery <$> HC.completeQueries query completions


selectPackageVersion :: T.Text -> App (Maybe T.Text)
selectPackageVersion packageName = do
  result <- runRequest $ HC.eval $ packageVersionCmd packageName
  case result of
    HC.ResSearch limitedResult -> do
      versionResult <- return $ decode $ encode $ HC.lrResult limitedResult
      return $ versionResult >>= packageVersion

    clientResult ->
      throwError $ InvalidCmdResult clientResult



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
      | otherwise = either (const []) (:[])            -- throw away errors
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



-- METRICS


newMetrics :: (MonadIO m) => Store -> m Metrics
newMetrics store = do
  searches    <- createMetric "searches" "searchStats" store
  completions <- createMetric "completions" "completionStats" store
  return $ Metrics searches completions


measure :: (Metrics -> Metric) -> App a -> App a
measure get action =
  asks (get . envMetrics) >>= measureAndStore action


metrics :: Store -> App Sample
metrics =
  collectStats



-- HELPERS


runRequest :: ClientM a -> App a
runRequest req = do
  client <- asks envClient
  result <- liftIO $ HC.runClientM req client
  either (throwError . HuntClientError) return result


packageVersion :: [PackageVersionResult] -> Maybe T.Text
packageVersion (PackageVersionResult v:[]) = Just v
packageVersion _                           = Nothing


removeQuotes :: T.Text -> T.Text
removeQuotes t
  | T.null t          = t
  | surroundedBy '"'  = T.dropAround (== '"') t
  | surroundedBy '\'' = T.dropAround (== '\'') t
  | otherwise         = t
  where
    surroundedBy c =
      T.head t == '"'  && T.last t == '"'


isSignatureQuery :: T.Text -> Bool
isSignatureQuery q =
  "->" `T.isInfixOf` q || "=>" `T.isInfixOf` q
