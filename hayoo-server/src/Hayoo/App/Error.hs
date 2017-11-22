module Hayoo.App.Error
  ( Error (..)
  ) where


import qualified Hunt.ClientInterface as HC
import           Servant.Client       (ServantError)



-- ERROR


data Error
  = HuntClientError ServantError
  | InvalidCmdResult HC.CmdResult
  deriving (Show, Eq)
