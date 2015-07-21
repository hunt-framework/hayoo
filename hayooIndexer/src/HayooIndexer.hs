{-# LANGUAGE FlexibleContexts #-}
-- ------------------------------------------------------------

module Main (main)
where

-- force NewPkgIndexerCore to get compiled
import qualified Hayoo.Hunt.NewPkgIndexerCore as ZZZ

import           Codec.Compression.BZip     (compress, decompress)

import           Control.Applicative        ((<$>))
import           Control.Monad.Error
import           Control.Monad.IO.Class     ()
import           Control.Monad.Reader

import           Data.Char
import           Data.Function.Selector
import           Data.Time                  (getCurrentTime)

import           Hayoo.HackagePackage
import           Hayoo.Haddock
import           Hayoo.IndexConfig
import           Hayoo.PackageArchive
import           Hayoo.URIConfig

import qualified Hayoo.Hunt.FctIndexerCore  as FJ
import           Hayoo.Hunt.FctRankTable
import           Hayoo.Hunt.IndexSchema
import           Hayoo.Hunt.Output          (defaultServer, evalOkRes,
                                             outputValue)
import qualified Hayoo.Hunt.PkgIndexerCore  as PJ
import           Hayoo.Hunt.PkgRankTable    as PR

import           Holumbus.Crawler
import           Holumbus.Crawler.CacheCore
import           Holumbus.Crawler.IndexerCore

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Text.XML.HXT.Cache
import           Text.XML.HXT.Core
import           Text.XML.HXT.Curl
import           Text.XML.HXT.HTTP          ()


-- ------------------------------------------------------------

data AppAction
    = Usage
    | BuildIx | UpdatePkg | BuildCache
    | CreateSchema | DeleteSchema
    | JsonAll | JsonUpdate | JsonRank
      deriving (Eq, Show)

data AppOpts
    = AO
      { ao_progname    :: String
      , ao_pkgIndex    :: Bool
      , ao_JSONserv    :: Maybe String
      , ao_JSONmax     :: Int
      , ao_JSONmaxsave :: Maybe Int
      , ao_action      :: AppAction
      , ao_packages    :: Maybe [String]
      , ao_pkgregex    :: Maybe String
      , ao_latest      :: Maybe Int
      , ao_getHack     :: Bool
      , ao_msg         :: String
      , ao_crawlDoc    :: (Int, Int, Int)
      , ao_crawlSav    :: Int
      , ao_crawlSfn    :: String
      , ao_crawlLog    :: (Priority, Priority)
      , ao_crawlPar    :: SysConfig
      , ao_crawlCch    :: CacheCrawlerConfig    -> CacheCrawlerConfig
      , ao_crawlPkJ    :: PJ.PkgCrawlerConfig   -> PJ.PkgCrawlerConfig
      , ao_crawlFcJ    :: FJ.FctCrawlerConfig   -> FJ.FctCrawlerConfig
      }

-- ------------------------------------------------------------

initAppOpts :: AppOpts
initAppOpts
    = AO
      { ao_progname     = "hayooCrawler"
      , ao_pkgIndex     = False
      , ao_JSONserv     = Nothing
      , ao_JSONmax      = 1
      , ao_JSONmaxsave  = Nothing
      , ao_action       = Usage
      , ao_packages     = Nothing
      , ao_pkgregex     = Nothing
      , ao_latest       = Nothing
      , ao_getHack      = False
      , ao_msg          = ""
      , ao_crawlDoc     = (50000, 1024, 1)                                          -- max docs, max par docs, max threads: no parallel threads, but 1024 docs are indexed before results are inserted
      , ao_crawlSav     = 5000                                                      -- save intervall
      , ao_crawlSfn     = "./tmp/ix-"                                               -- save path
      , ao_crawlLog     = (DEBUG, NOTICE)                                           -- log cache and hxt
      , ao_crawlPar     = withCache' (60 * 60 * 24 * 30)                            -- set cache dir, cache remains valid 1 month, 404 pages are cached
                          >>>
                          withCompression (compress, decompress)                    -- compress cache files
                          >>>
                          withStrictDeserialize yes                                 -- strict input of cache files
                          >>>
                          withAcceptedMimeTypes [ text_html
                                                , application_xhtml
                                                ]
                          >>>
                          withCurl [ (curl_location,             v_1)               -- automatically follow redirects
                                   , (curl_max_redirects,        "3")               -- but limit # of redirects to 3
                                   ]
                          >>>
                          -- withHTTP [ (curl_max_redirects,        "3") ]          -- nice try: HTTP web access instead of curl, problem: no document size limit
                          -- >>>
                          withRedirect yes
                          >>>
                          withInputOption curl_max_filesize
                                              (show (1024 * 1024 * 3 `div` 2 ::Int)) -- this limit excludes automtically generated pages, sometimes > 1.5 Mbyte
                          >>>
                          withParseHTML no
                          >>>
                          withParseByMimeType yes

      , ao_crawlCch     = ( editPackageURIs                                         -- configure URI rewriting
                            >>>
                            disableRobotsTxt                                        -- for hayoo robots.txt is not needed
                          )

      , ao_crawlPkJ     = disableRobotsTxt

      , ao_crawlFcJ     = ( editPackageURIs                                         -- configure URI rewriting
                            >>>
                            disableRobotsTxt                                        -- for hayoo robots.txt is not needed
                          )
      }
    where
      editPackageURIs
          = chgS theProcessRefs (>>> arr editLatestPackage)

withCache' :: Int -> XIOSysState -> XIOSysState
withCache' sec
    = withCache "./cache" sec yes

-- ------------------------------------------------------------

type HIO = ReaderT AppOpts (ErrorT String IO)

main :: IO ()
main
    = do pn   <- getProgName
         args <- getArgs
         res  <- runErrorT $ runReaderT main2 (evalOptions pn args)
         either (const exitFailure) (const exitSuccess) res

-- ------------------------------------------------------------

main2 :: HIO ()
main2
    = do (a, pn) <- asks (ao_action &&& ao_progname)
         case a of
           Usage
               -> do msg <- asks ao_msg
                     liftIO $ hPutStrLn stderr (msg ++ "\n" ++ usageInfo pn hayooOptDescr)
                     if null msg
                       then return ()
                       else throwError "wrong option"
           _   -> do asks (snd . ao_crawlLog) >>= setLogLevel ""
                     asks ao_getHack          >>= getHackageIndex
                     local (\ opts -> opts {ao_getHack = False}) $
                       case a of
                         BuildCache   -> mainCache
                         CreateSchema -> indexSchema execCreateHayooIndexSchema
                         DeleteSchema -> indexSchema execDropHayooIndexSchema
                         JsonRank     -> jsonRank
                         JsonAll      -> mainJsonAll
                         JsonUpdate   -> mainJsonUpdate
                         _            -> do p <- asks ao_pkgIndex
                                            if p
                                              then mainHackageJSON
                                              else mainHaddockJSON

-- ------------------------------------------------------------

getHackageIndex :: Bool -> HIO ()
getHackageIndex False
    = return ()
getHackageIndex True
    = do notice ["update 00-index.tag.gz from hackage"]
         liftIO $ updateArchiveFile

indexSchema :: (Maybe String -> HIO ()) -> HIO ()
indexSchema out
    = asks ao_JSONserv >>= out

-- ------------------------------------------------------------

-- | macro for computing whole hayoo index in JSON format
-- cache can be updated on the fly by setting --valid=0sec

mainJsonAll :: HIO ()
mainJsonAll
    = local ( \ x ->
              x { ao_action      = BuildIx
                , ao_pkgIndex    = False
                , ao_latest      = Nothing      -- no package selection by latest
                , ao_packages    = Nothing      -- no selection by package list
                , ao_pkgregex    = Nothing      -- and no selection by regex,
                }                               -- all packages are taken
            ) $ do indexSchema                  -- create index schema
                     execCreateHayooIndexSchema
                   jsonHackage                  -- create the package index
                   jsonRank                     -- create the package rank
                   mainHaddockJSON              -- the main step: index the haddock pages


mainJsonUpdate :: HIO ()
mainJsonUpdate
    = do latest <- maybe defLatest Just <$> asks ao_latest
         local ( \ x ->
                 x { ao_action      = BuildIx
                   , ao_pkgIndex    = False
                   , ao_latest      = latest       -- select packages by age
                   , ao_packages    = Nothing      -- no selection by package list
                   , ao_pkgregex    = Nothing      -- and no selection by regex,
                   , ao_crawlPar    = setDocAge 0 (ao_crawlPar x)
                   }                               -- update cache for latest packages
               ) $ withPackages False $
                   do jsonHackage                  -- create the package index
                      jsonRank                     -- create the package rank
                      mainHaddockJSON              -- the main step: index the haddock pages
    where
      defLatest = Just $ 60 * 60 * 24   -- 1 day

jsonHackage :: HIO ()
jsonHackage
    = local (\ x -> x { ao_pkgIndex    = True })
      mainHackageJSON

jsonRank :: HIO ()
jsonRank
    = do serv <- asks ao_JSONserv
         save <- maybe False (const True) <$>
                 asks ao_JSONmaxsave
         now  <- liftIO getCurrentTime
         notice ["computing JSON package ranks"]
         rank <- liftIO $ PR.toCommand save now (src serv)
         outputValue (dest serv) rank >>= evalOkRes
         notice ["JSON package ranks written"]
    where
      dest Nothing    = Left "02-ranking"
      dest (Just uri) = Right uri

      src  Nothing    = Left "01-packages"
      src  (Just uri) = Right uri

-- ------------------------------------------------------------

mainCache :: HIO ()
mainCache
    = do action
    where
      action
          = withPackages False action2

      action2
          = do pl <- asks ao_packages
               case pl of
                 Nothing -> action3
                 Just xs -> updatePkg xs

      action3
          = do notice ["cache hayoo pages"]
               _ <- hayooCacher
               notice ["cache update for hayoo pages done"]

      updatePkg []
          = notice ["no packages to be updated"]
      updatePkg ps
          = do notice $ "updating cache with packages:" : ps
               _ <- hayooPackageUpdate ps
               notice $ "updating cache with latest packages done" : []

-- ------------------------------------------------------------

mainHackageJSON :: HIO ()
mainHackageJSON
    = do action
    where
      action
          = withPackages True action2

      action2
          = do act <- asks ao_action
               ps  <- (maybe [] id) <$> asks ao_packages
               case act of
                 BuildIx
                     -> indexPkg ps
                 _   -> notice ["mainHackageJSON:", "ignoring command"]

      indexPkg :: [String] -> HIO ()
      indexPkg ps
          = do notice $ if null ps
                          then ["JSON indexing all packages from hackage package index"]
                          else "JSON indexing hackage package descriptions for packages:" : ps
               ix <- getS theResultAccu <$> hayooPJIndexer
               notice ["writing package index as JSON"]
               flushJSON "01-packages" ix
               notice ["package index as JSON written"]

      flushJSON :: String -> PJ.PkgIndexerState -> HIO ()
      flushJSON pkg ix
          = do serv <- asks ao_JSONserv
               save <- maybe False (const True) <$> asks ao_JSONmaxsave
               ct <- liftIO $ getCurrentTime
               notice $ "flushing package index as JSON to" : target serv
               outputValue (dest serv) (PJ.toCommand save ct True ix) >>= evalOkRes
               notice $ ["flushing package index as JSON done"]
               return ()
          where
            target (Just uri) = ["server", show uri]
            target  Nothing   = ["file",   show pkg]
            dest  (Just uri)  = Right uri
            dest   Nothing    = Left pkg

-- ------------------------------------------------------------

mainHaddockJSON :: HIO ()
mainHaddockJSON
    = withPackages True action
    where
      action
          = do act <- asks ao_action
               ps  <- (maybe [] id) <$>
                      asks ao_packages
               case act of
                 BuildIx
                     -> indexPkgList ps
                 _   -> notice ["mainHaddockJSON:", "ignoring command"]

      getPkgRank :: HIO FctRankTable
      getPkgRank
          = asks ao_JSONserv >>= rankFrom
          where
            rankFile = "json/02-ranking.js"
            rankFrom Nothing    = do notice [ "mainHaddockJSON:"
                                            , "reading package rank from file"
                                            , show rankFile
                                            ]
                                     liftIO $ rankFromFile rankFile
            rankFrom (Just uri) = do notice [ "mainHaddockJSON:"
                                            , "reading package rank from server"
                                            , show uri
                                            ]
                                     rnk <- liftIO $ rankFromServer uri
                                     notice [ "mainHaddockJSON:"
                                            , "rank table from server:\n"
                                            , show rnk
                                            ]
                                     return rnk

      indexPkgList :: [String] -> HIO ()
      indexPkgList []  = noaction
      indexPkgList ps' = do mx <- asks ao_JSONmax
                            ms <- maybe maxBound id <$> asks ao_JSONmaxsave
                            rt <- getPkgRank
                            sequence_ . (map $ uncurry (indexPkg rt)) $ part mx ms ps'
          where
            part :: Int -> Int -> [String] -> [(Bool, [String])]
            part n s = part' 0
                where
                  part' _ [] = []
                  part' i xs = (b, x) : part' i' xs'
                      where
                        (x, xs') = splitAt n xs
                        i' = if b then 0 else i + n
                        b  = (i + n >= s)                -- maximum reached
                             ||
                             (s /= maxBound && null xs') -- last chunk

      indexPkg :: FctRankTable -> Bool -> [String] -> HIO ()
      indexPkg rt save pkgs
          = do notice ["start indexing haddock pages in JSON for package:", show pkgs]
               local (\ o -> o { ao_packages = Just pkgs }
                     ) $ do ix <- getS theResultAccu <$> hayooFJIndexer
                            flushJSON rt save pkgs ix

               notice ["finish indexing haddock pages in JSON for package:", show pkgs]
               return ()

      flushJSON :: FctRankTable -> Bool -> [String] -> FJ.FctIndexerState -> HIO ()
      flushJSON rt save pkgs ix
          = do serv <- asks ao_JSONserv
               ct <- liftIO $ getCurrentTime
               notice $ "flushing function index as JSON to" : target serv
               outputValue (dest serv) (FJ.toCommand rt save ct True pkgs ix) >>= evalOkRes
               notice $ ["flushing function index as JSON done"]
               return ()
          where
            target (Just uri) = ["server", show uri]
            target  Nothing   = ["file",   show $ fn pkgs]
            dest  (Just uri)  = Right uri
            dest   Nothing    = Left $ fn pkgs
            fn [pkg]          = pkg
            fn xs             = head xs ++ ".." ++ last xs

-- ------------------------------------------------------------

noaction :: HIO ()
noaction
    = notice ["no packages to be processed"]

-- ------------------------------------------------------------

getPackages :: Bool -> HIO (Maybe [String])
getPackages allPkgs
    = do pl <- asks ao_packages
         r  <- asks ao_pkgregex
         ls <- asks ao_latest
         pl1 <- packageList pl ls
         pl2 <- filterRegex r pl1
         case pl2 of
           Just xs -> notice ["packages to be processed:", show xs]
           Nothing -> notice ["all packages to be processed"]
         return pl2
    where
      packageList :: Maybe [String] -> Maybe Int -> HIO (Maybe [String])
      packageList _ (Just age)                                -- eval option --latest
          = do notice ["compute list of latest packages"]
               res <- liftIO $ getNewPackages age
               notice ["latest packages:", show res]
               return $ Just res

      packageList Nothing _                                   -- compute default package list
          | allPkgs
              = Just <$> (liftIO $ getNewPackages 0)
          | otherwise
              = return Nothing

      packageList ps _                                        -- explicit list --packages
          = return ps

      filterRegex :: Maybe String -> Maybe [String] -> HIO (Maybe [String])
      filterRegex _ Nothing
          = return Nothing

      filterRegex Nothing ps
          = return ps

      filterRegex (Just rex) (Just ps)
          = do notice ["filtered package list:", show ps]
               return $ Just res
            where
              res = filter (match rex) ps

-- | compute packages to be updated by evaluating
-- options --latest, --pkg-regex and --packages
-- and perform an action with these packages

withPackages :: Bool -> HIO a -> HIO a
withPackages allPkgs act
    = do ps <- getPackages allPkgs
         local (\opts -> opts
                         { ao_latest   = Nothing
                         , ao_pkgregex = Nothing
                         , ao_packages = ps
                         }
               ) act

-- ------------------------------------------------------------

hayooCacher :: HIO CacheCrawlerState
hayooCacher
    = do o <- ask
         liftIOC $ stdCacher
                    (ao_crawlDoc o)
                    (ao_crawlSav o, ao_crawlSfn o)
                    (ao_crawlLog o)
                    (ao_crawlPar o)
                    (ao_crawlCch o)
                    Nothing
                    hayooStart
                    (hayooRefs True [])

liftIOC :: IO (Either String a) -> HIO a
liftIOC action
    = liftIO action >>= either throwError return

-- ------------------------------------------------------------

hayooPackageUpdate :: [String] -> HIO CacheCrawlerState
hayooPackageUpdate pkgs
    = do o <- ask
         liftIOC $ stdCacher
                    (ao_crawlDoc o)
                    (ao_crawlSav o, ao_crawlSfn o)
                    (ao_crawlLog o)
                    (ao_crawlPar o)
                    (ao_crawlCch o)
                    Nothing
                    hayooStart
                    (hayooRefs True pkgs)

-- ------------------------------------------------------------
--
-- the JSON package indexer

hayooPJIndexer :: HIO PJ.PkgCrawlerState
hayooPJIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    Nothing
                    hackageStart
                    PJ.emptyPkgState
    where
      config0 o
          = PJ.indexCrawlerConfig
            (ao_crawlPar o)
            (hayooRefs False $ (maybe [] id $ ao_packages o))
            Nothing
            (Just $ checkDocumentStatus >>> preparePkg)
            (Just $ hayooGetPkgTitle)
            (Just $ hayooGetPkgInfo)
            hayooPkgIndexContextConfig

      config o
          = ao_crawlPkJ o $
            setCrawlerTraceLevel ct ht   $
            setCrawlerSaveConf si sp     $
            setCrawlerMaxDocs md mp mt   $
            config0                      $ o
          where
            (ct, ht)      = ao_crawlLog o
            si            = ao_crawlSav o
            sp            = ao_crawlSfn o
            (md, mp, mt)  = ao_crawlDoc o

-- ------------------------------------------------------------
--
-- the JSON package indexer

hayooFJIndexer :: HIO FJ.FctCrawlerState
hayooFJIndexer
    = do o <- ask
         liftIOC $ stdIndexer
                    (config o)
                    Nothing
                    hayooStart
                    FJ.emptyFctState
    where
      config0 o
          = FJ.indexCrawlerConfig
            (ao_crawlPar o)
            (hayooRefs True $ (maybe [] id $ ao_packages o))
            Nothing
            (Just $ checkDocumentStatus >>> prepareHaddock)
            (Just $ hayooGetTitle)
            (Just $ hayooGetFctInfo FJ.rewriteHrefs)
            hayooIndexContextConfig

      config o
          = ao_crawlFcJ o $
            setCrawlerTraceLevel ct ht   $
            setCrawlerSaveConf si sp     $
            setCrawlerMaxDocs md mp mt   $
            setCrawlerPreRefsFilter noHaddockPage $
            config0                      $ o
          where
            (ct, ht)      = ao_crawlLog o
            si            = ao_crawlSav o
            sp            = ao_crawlSfn o
            (md, mp, mt)  = ao_crawlDoc o

-- ------------------------------------------------------------

noHaddockPage :: IOSArrow XmlTree XmlTree
noHaddockPage
    = fromLA $
      hasAttrValue transferURI (not . isHaddockURI) `guards` this

-- ------------------------------------------------------------

notice :: MonadIO m => [String] -> m ()
notice = noticeC "hayoo"

{-
critical :: MonadIO m => [String] -> m ()
critical = errC "hayoo"
-- -}
-- ------------------------------------------------------------

evalOptions :: String -> [String] -> AppOpts
evalOptions pn args
    = foldl (.) (ef1 . ef2) opts $ initAppOpts { ao_progname = pn }
    where
    (opts, ns, es)   = getOpt Permute hayooOptDescr args
    ef1
        | null es    = id
        | otherwise  = \ x -> x { ao_action = Usage
                                , ao_msg    = concat es
                                }
        | otherwise  = id
    ef2
        | null ns    = id
        | otherwise  = \ x -> x { ao_action = Usage
                                , ao_msg = "wrong program arguments: " ++ unwords ns
                                }

-- ------------------------------------------------------------

hayooOptDescr :: [OptDescr (AppOpts -> AppOpts)]
hayooOptDescr
    = [ Option "h?" ["help"]
        ( NoArg $
          \ x -> x { ao_action = Usage }
        )
        "usage info"

      , Option "" ["fct-index"]
        ( NoArg $
          \ x -> x { ao_action = BuildIx
                   , ao_pkgIndex = False
                   , ao_crawlSfn = "./tmp/ix-"
                   }
        )
        "process index for haddock functions and types (default)"

      , Option "" ["pkg-index"]
        ( NoArg $
          \ x -> x { ao_action = BuildIx
                   , ao_pkgIndex = True
                   , ao_crawlSfn = "./tmp/pkg-"
                   }
        )
        "process index for hackage package description pages"

      , Option "" ["cache"]
        ( NoArg $
          \ x -> x { ao_action   = BuildCache }
        )
        "update the cache"

      , Option "p" ["packages"]
        ( ReqArg
          (\ l x -> x { ao_packages = Just $ pkgList l }
          )
          "PACKAGE-LIST"
        )
        "packages to be processed, a comma separated list of package names"

      , Option "" ["pkg-regex"]
        ( ReqArg
          (\ l x -> x { ao_pkgregex = Just l }
          )
          "PACKAGE-REGEX"
        )
        "filter for packages to be processed, a regular expression pattern)"

      , Option "u" ["update"]
        ( NoArg $
          \ x -> x { ao_action   = UpdatePkg }
        )
        "update packages specified by \"packages\" option"

      , Option "" ["json-all"]
        ( NoArg $
          \ x -> x { ao_action   = JsonAll
                   }
        )
        ( unwords [ "JSON macro command to create complete Hayoo index in JSON format,"
                  , "(--json-create-schema, --json-pkg, --json-rank, --json-fct)"
                  ]
        )

      , Option "" ["json-update"]
        ( NoArg $
          \ x -> x { ao_action   = JsonUpdate
                   , ao_getHack  = True
                   }
        )
        ( unwords [ "JSON macro command to update Hayoo index with latest packages,"
                  , "with option --latest the age of the packages may be set, default is 1 day"
                  ]
        )

      , Option "" ["json-create-schema"]
        ( NoArg $
          \ x -> x { ao_action   = CreateSchema
                   }
        )
        "JSON command to create Hayoo index schema in Hunt server (--json-output implied)"

      , Option "" ["json-delete-schema"]
        ( NoArg $
          \ x -> x { ao_action   = DeleteSchema
                   }
        )
        "JSON command to drop Hayoo index schema in Hunt server (--json-output implied)"

      , Option "" ["json-fct"]
        ( NoArg $
          \ x -> x { ao_action      = BuildIx
                   , ao_pkgIndex    = False
                   }
        )
        "JSON command to index Haddock document pages in Hunt server (--json-output implied)"

      , Option "" ["json-pkg"]
        ( NoArg $
          \ x -> x { ao_action      = BuildIx
                   , ao_pkgIndex    = True
                   }
        )
        "JSON command to index Hayoo package descriptions in Hunt server (--json-output implied)"

      , Option "" ["json-pkg-rank"]
        ( NoArg $
          \ x -> x { ao_action      = JsonRank
                   }
        )
        "JSON command to compute Hayoo package rank in Hunt server (--json-output implied)"

      , Option "" ["json-server"]
        ( ReqArg
          (\ u x -> x { ao_JSONserv = Just $ if null u then defaultServer else u})
          "URI"
        )
        ( "the server, into which the JSON output will be pushed, default is " ++
          show defaultServer ++ " (no file output)"
        )

      , Option "" ["json-maxpkg"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_JSONmax = 1 `max` i }))
          "NUMBER"
        )
        "when indexing JSON, maximum # of packages indexed as a bundle, default is 1"

      , Option "" ["json-maxsave"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_JSONmaxsave = Just $ 1 `max` i }))
          "NUMBER"
        )
        "when indexing JSON, saving server state after indexing # of packages, default: infinity"

      , Option "" ["maxdocs"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxDocs i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        "maximum # of docs to be processed"

      , Option "" ["maxthreads"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxThreads i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        ( "maximum # of parallel threads, 0: sequential, 1: single thread with binary merge," ++
          " else real parallel threads, default: 1" )

      , Option "" ["maxpar"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlDoc = setMaxParDocs i $
                                                                 ao_crawlDoc x }))
          "NUMBER"
        )
        "maximum # of docs indexed at once before the results are inserted into index, default: 1024"

      , Option "" ["valid"]
        ( ReqArg (setOption parseDuration (\ x t -> x { ao_crawlPar = setDocAge t $
                                                                      ao_crawlPar x }))
          "DURATION"
        )
        ( "validate cache for pages older than given age, format: " ++
          "10sec, 5min, 20hours, 3days, 5weeks, 1month, default is 1month" )

      , Option "" ["latest"]
        ( ReqArg (setOption parseDuration (\ x t -> x { ao_latest   = Just t }))
          "DURATION"
        )
        "select latest packages newer than given age, format like in option \"valid\""

      , Option "" ["save"]
        ( ReqArg (setOption parseInt (\ x i -> x { ao_crawlSav  = i }))
          "NUMBER"
        )
        "save intermediate results of index, default is 5000"

      , Option "" ["hackage"]
        ( NoArg $
          \ x -> x { ao_getHack   = True }
        )
        "when processing latest packages, first update the package list from hackage"
      ]
    where
    pkgList
        = words . map (\ x -> if x == ',' then ' ' else x)

    setOption parse f s x
        = either (\ e -> x { ao_msg    = e
                           , ao_action = Usage
                           }
                 ) (f x) . parse $ s

-- ------------------------------------------------------------

parseInt                                :: String -> Either String Int
parseInt s
    | match "[0-9]+" s                  = Right $ read s
    | otherwise                         = Left  $ "number expected in option arg"

parseDuration                           :: String -> Either String Int
parseDuration s
    | match "[0-9]+(s(ec)?)?"      s    = Right $ t
    | match "[0-9]+(m(in)?)?"      s    = Right $ t * 60
    | match "[0-9]+(h(our(s)?)?)?" s    = Right $ t * 60 * 60
    | match "[0-9]+(d(ay(s)?)?)?"  s    = Right $ t * 60 * 60 * 24
    | match "[0-9]+(w(eek(s)?)?)?" s    = Right $ t * 60 * 60 * 24 * 7
    | match "[0-9]+(m(onth(s)?)?)?" s   = Right $ t * 60 * 60 * 24 * 30
    | match "[0-9]+(y(ear(s)?)?)?" s    = Right $ t * 60 * 60 * 24 * 30 * 365
    | otherwise                         = Left  $ "error in duration format in option arg"
    where
    t                                   = read . filter isDigit $ s

-- ------------------------------------------------------------

setMaxDocs                              :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxDocs    md (_md, mp, mt)          = (md, md `min` mp, mt)

setMaxParDocs                           :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxParDocs mp (md, _mp, mt)          = (md, mp, mt)

setMaxThreads                           :: Int -> (Int, Int, Int) -> (Int, Int, Int)
setMaxThreads mt (md, mp, _mt)          = (md, mp, mt)

setDocAge                               :: Int -> SysConfig -> SysConfig
setDocAge d                             = (>>> withCache' d)

-- ------------------------------------------------------------
