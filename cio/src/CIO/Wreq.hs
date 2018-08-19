{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module CIO.Wreq
  ( module CIO.Wreq
  , module Network.Wreq
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.ByteString.Lens
import Data.Conduit
import Data.Default
import GHC.Generics (Generic)
import Network.Wreq hiding (getWith, get, responseLink, Response, responseBody)
import CIO.Types

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Database.LevelDB as DB
import qualified Network.Wreq as Wreq

data Response = Response
    { _responseLink :: Maybe String
    , _responseBody :: BL.ByteString }
    deriving (Generic, Show)

instance B.Binary Response

makeLenses ''Response

get :: String -> CIO Response
get = getWith Wreq.defaults

getWith :: Options -> String -> CIO Response
getWith opts url = do
    db <- ask
    DB.get db def key >>= \case
        Just resp -> pure $ B.decode $ BL.fromStrict resp
        Nothing -> do
            x <- liftIO $ Wreq.getWith opts url
            let resp = Response
                    { _responseBody = (x^.Wreq.responseBody)
                    , _responseLink = (x^?Wreq.responseLink "rel" "next".linkURL.unpackedChars)
                    }
            DB.put db def key (BL.toStrict $ B.encode resp)
            pure resp
  where
    key = hashRequest opts url

hashRequest :: Options -> String -> BS.ByteString
hashRequest opts url = SHA256.hash (BS8.pack $ show (opts, url))

getAllWith :: Options -> String -> Producer CIO Response
getAllWith = fix $ \loop opts url -> do
    resp <- lift $ getWith opts url
    yield resp
    mapM_ (loop (opts&params.~[])) (_responseLink resp)

dirtyReq :: String -> CIO ()
dirtyReq = dirtyReqWith Wreq.defaults

dirtyReqWith :: Options -> String -> CIO ()
dirtyReqWith opts url =  do
    db <- ask
    DB.delete db def (hashRequest opts url)
