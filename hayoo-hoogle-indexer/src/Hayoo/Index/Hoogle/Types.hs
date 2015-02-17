module Hayoo.Index.Hoogle.Types where

type PackageName = String
type Version = String
type ModuleName = String
type Anchor = String

type MkURI a = PackageName -> Version -> ModuleName -> a -> (a -> Anchor) -> String
