{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- ------------------------------------------------------------

module Hayoo.FunctionInfo
    ( FunctionInfo(..)
    , Fct'Type(..)
    , mkFunctionInfo
    , fromFct'Type
    , Score
    )
where
import           Control.Applicative ((<$>), (<*>))
import           Control.DeepSeq     (NFData, rnf)

import           Data.Aeson          (ToJSON, object, toJSON, (.=))
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as B
import           Data.Maybe          (fromMaybe)
import           Data.Typeable       (Typeable)

-- ------------------------------------------------------------

-- | Additional information about a function.

type Score = Float

data FunctionInfo
    = FunctionInfo
      { moduleName :: String               -- ^ The name of the module containing the function, e.g. Data.Map
      , signature  :: String               -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
      , package    :: String               -- ^ The name of the package containing the module, e.g. containers
      , sourceURI  :: String               -- ^ An optional URI to the online source of the function.
      , fctDescr   :: String               -- ^ The haddock description of a type or function, maybe shortened for space efficiency
      , fctType    :: ! Fct'Type           -- ^ The type of the documented part, function, class, type, ...
      , docURI     :: String               -- ^ A URI associated with document (i.e. link to the function's documentation)
      }
    deriving (Show, Eq, Typeable)

data Fct'Type
    = Fct'class
    | Fct'data
    | Fct'function
    | Fct'method
    | Fct'module
    | Fct'newtype
    | Fct'type
    | Fct'unknown
      deriving (Show, Eq, Enum, Bounded, Typeable)

fctAssocList :: [(String, Fct'Type)]
fctAssocList = map (\ x -> (drop 4 . show $ x, x)) [minBound..maxBound]

toFct'Type :: String -> Fct'Type
toFct'Type s = fromMaybe Fct'unknown $ lookup s fctAssocList

fromFct'Type :: Fct'Type -> String
fromFct'Type = drop 4 . show

-- mkFunctionInfo is a strict constructor

mkFunctionInfo                  :: String -> String -> String -> String -> String -> String -> String
                                   -> FunctionInfo
mkFunctionInfo m s p r d t r'   = let res = FunctionInfo m s p r d (toFct'Type t) r' in
                                  rnf res `seq` res

instance NFData FunctionInfo where
    rnf (FunctionInfo m s p r d _t r')
                                = rnf m `seq` rnf s `seq` rnf p `seq` rnf r `seq` rnf d `seq` rnf r'

instance B.Binary FunctionInfo where
    put (FunctionInfo m s p r d t r')
                                = put m >> put s >> put p >> put r >> put d >> put t >> put r'
    get                         = do
                                  r <- FunctionInfo <$> get <*> get <*> get <*> get <*> get <*> get <*> get
                                  rnf r `seq`
                                      return r

instance B.Binary Fct'Type where
    put = put . fromEnum
    get = toEnum <$> get

instance ToJSON FunctionInfo where
    toJSON (FunctionInfo mon sig pac sou fct typ docuri)
        = object
          ( map (uncurry (.=)) . filter (not . null . snd)
            $ [ ("fct-module",    mon)
              , ("fct-signature", sig)
              , ("fct-package",   pac)
              , ("fct-source",    sou)
              , ("fct-descr",     fct)
              , ("fct-type",      fromFct'Type typ)
              , ("fct-docuri",    docuri)
              ]
          )

-- ------------------------------------------------------------
