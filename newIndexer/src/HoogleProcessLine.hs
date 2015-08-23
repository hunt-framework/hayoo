{-# LANGUAGE NoMonomorphismRestriction #-}

module HoogleProcessLine
where

import Control.Monad.State.Strict

import Hayoo.FunctionInfo
import ParseHoogle (HoogleLine(..))
import qualified ParseHoogle as PH
import Text.Show.Pretty (ppShow)

import Data.Char (isSpace,ord,isAlphaNum)
import Data.List

data HState = HState { h_moduleName :: String    -- current module
                     , h_package    :: String    -- current package
                     , h_comments   :: [String]  -- comment lines preceding a definition
                     , h_uriPrefix  :: String    -- current uri prefix
                     }

emptyHState = HState "" "" [] ""

addComment s = modify (\hs -> hs { h_comments = (s:(h_comments hs)) } )
setPackage s = modify (\hs -> hs { h_package = s })
setModuleName s  = modify (\hs -> hs { h_moduleName = s })
clearComments = modify (\hs -> hs { h_comments = [] })

fixupComments :: [String] -> String
fixupComments xs = unlines $ map go xs
  where go = dropBar
        dropBar ('|':' ':xs) = xs
        dropBar xs           = xs

fixupSignature :: String -> String
fixupSignature = removeString "{- UNPACK -}" . removeBang
  where
    removeBang = filter (/='!')

removeString substr str = go str
  where go [] = []
        go str@(c:cs)
          | substr `isPrefixOf` str = ' ' : go (drop n str)
          | otherwise               = c : go cs
        n = length substr

makeFunctionInfo kind name signature uriSuffix = do
  hs <- get
  let comments = fixupComments . reverse . h_comments $ hs
      docuri = h_uriPrefix hs ++ uriSuffix
      -- n.b. sourceURI is left empty, we only use docURI
      fi = mkFunctionInfo (h_moduleName hs) signature (h_package hs) "" comments kind docuri
  clearComments
  return fi

toUri :: String -> String
toUri name = concatMap go name
  where go ch | isAlphaNum ch = [ch]
              | otherwise     = "-" ++ show (ord ch) ++ "-"

typeUri name = "#t:" ++ toUri name
funcUri name = "#v:" ++ toUri name

processLine _ BlankLine    = return ()
processLine _ (Comment s)  = addComment s
processLine _ (Package s)  = setPackage s
processLine _ (Version s)  = return ()
processLine next (Module s)   = do
  setModuleName s
  hs <- get
  let prefix = "http://hackage.haskell.org/package/" ++ (h_package hs) ++ "/docs/" ++ moduleDashed ++ ".html"
      moduleDashed = map (replaceDot '-') (h_moduleName hs)
        where replaceDot ch '.' = ch
              replaceDot _  x   = x
  put $ hs { h_uriPrefix = prefix }
  emitFunctionInfo next "module" s "" "#"

processLine _ (Instance s) = return ()

processLine next (Type name lhs sig)     = emitFunctionInfo next "type"     name sig (typeUri name)
processLine next (Newtype name _)        = emitFunctionInfo next "newtype"  name ""  (typeUri name)
processLine next (FunctionDecl name sig) = emitFunctionInfo next "function" name sig (funcUri name)
processLine next (DataDecl name)         = emitFunctionInfo next "data"     name ""  (typeUri name)
processLine next (MultiDecl names sig)   = forM_ names $ \name -> emitFunctionInfo next "function" name sig (funcUri name)

processLine _ _          = return ()

emitFunctionInfo next kind name signature uri = do
  fi <- makeFunctionInfo kind name signature uri
  next (name, fi)
  -- liftIO $ putStrLn $ ppShow (name, fi)

