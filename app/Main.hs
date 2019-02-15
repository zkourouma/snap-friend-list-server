{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  ( mzero )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Value(Object)
                                                , (.:)
                                                , (.=)
                                                , encode
                                                , object
                                                , parseJSON
                                                , toJSON
                                                )
import           Snap.Core                      ( MonadSnap
                                                , Snap
                                                , getParam
                                                , ifTop
                                                , modifyResponse
                                                , setHeader
                                                , writeBS
                                                , writeLBS
                                                )
import           Snap.Http.Server               ( simpleHttpServe )
import           Snap.Http.Server.Config        ( Config
                                                , defaultConfig
                                                )
import           Paths_snap_friend_list_server

file :: IO FilePath
file = getDataFileName "data/friends.json"

friends :: IO [Friend]
friends = parseJSON <$> (readFile =<< file)

data Friend = Friend { friendId :: Integer, name  :: String, nickName :: String }  deriving (Show, Eq)

instance FromJSON Friend where
  parseJSON (Object v) = Friend <$> v .: "friendId" <*> v .: "name" <*> v .: "nickName"
  parseJSON _ = mzero

instance ToJSON Friend where
  toJSON (Friend fId n n') =
    object [ "friendId" .= fId, "name" .= n, "nickName" .= n']

main :: IO ()
main =
  (simpleHttpServe :: Config Snap a -> Snap () -> IO ()) defaultConfig site

site :: Snap ()
site = ifTop queryHandler

queryHandler :: Snap ()
queryHandler = do
  fs <- friends
  q <- getParam "q"
  maybe (writeBS fs) writeBS q

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON d = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ d
