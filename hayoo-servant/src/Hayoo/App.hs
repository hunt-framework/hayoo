{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hayoo.App
  ( -- * Types
    HayooApp (..)
  , HayooEnv (..)
  , HayooErr (..)
  ) where


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text            as T
import           Hayoo.App.Types
import qualified Hunt.Client          as HC
import qualified Servant.Client       as SC
import qualified Text.Parsec          as P


-- TYPES

newtype HayooApp a = HayooApp
  { unHayoo :: ReaderT HayooEnv (ExceptT HayooErr IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HayooEnv, MonadError HayooErr)


data HayooEnv = HayooEnv
  { envClient :: HC.HuntClient
  }


data HayooErr
  = ParserErr P.ParseError
  | HuntClientErr SC.ServantError
  deriving (Show)


-- ACTIONS


