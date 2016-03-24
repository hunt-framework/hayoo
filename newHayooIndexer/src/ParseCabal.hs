module ParseCabal
where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version

import Data.List (intercalate)

import PackageInfo

parseCabal cbl =
  case parsePackageDescription cbl of
    ParseFailed _ -> Nothing
    ParseOk _ d   -> let pinfo = PackageInfo
                                 { p_name         = extractPackageName d
                                 , p_version      = intercalate "." $ map show (extractPackageVersion d)
                                 , p_dependencies = extractDeps d
                                 , p_author       = extractAuthor d
                                 , p_maintainer   = extractMaintainer d
                                 , p_category     = extractCategory d
                                 , p_homepage     = extractHomePage d
                                 , p_synopsis     = extractSynopsis d
                                 , p_description  = extractDescription d
                                 , p_uploaddate   = ""
                                 , p_rank         = 0
                                 }
                     in Just pinfo

extractPackageName = go . pkgName . package . packageDescription
  where go (PackageName x) = x

extractPackageVersion = versionBranch . pkgVersion . package . packageDescription

extractAuthor = author . packageDescription

extractMaintainer = maintainer . packageDescription

extractCategory = category . packageDescription

extractHomePage = homepage . packageDescription

extractSynopsis = synopsis . packageDescription

extractDescription = description . packageDescription

getPackageName (Dependency (PackageName pkgname) _) = pkgname

extractDeps :: GenericPackageDescription -> [String]
extractDeps d =  map getPackageName (ldeps ++ edeps)
  where ldeps = case (condLibrary d) of
                  Nothing -> []
                  Just c  -> condTreeConstraints c
        edeps = concatMap (condTreeConstraints . snd) $ condExecutables d

