{-# LANGUAGE Rank2Types #-}
module Hayoo.Index.Conduit where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Conduit
import           Data.ByteString.Lazy (ByteString)
import qualified Data.String.UTF8 as UTF8

utf8 :: ByteString -> String
utf8 = UTF8.toString . UTF8.fromRep

compressedArchive :: Monad m
                  => ByteString
                  -> Producer m (FilePath, ByteString)
compressedArchive = unfoldC go . Tar.read . GZip.decompress
  where
    go (Tar.Next entry next) =
      case Tar.entryContent entry of
        Tar.NormalFile content _ ->
          Just ((Tar.entryPath entry, content), next)
        _                        -> go next
    go Tar.Done     = Nothing
    go (Tar.Fail e) = error $ "compressedArchive: " ++ show e

leftLogger :: (MonadIO m, Show a) => String -> Conduit (Either a  b) m b
leftLogger prefix = awaitForever $ \e ->
  case e of
   Left err -> liftIO $ putStrLn (prefix ++ show err)
   Right x  -> yield x
