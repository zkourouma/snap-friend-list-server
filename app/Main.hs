{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either                    ( fromRight )
import           Snap.Core                      ( MonadSnap
                                                , Snap
                                                , getParam
                                                , ifTop
                                                , modifyResponse
                                                , setHeader
                                                , writeLBS
                                                )
import           Snap.Http.Server               ( simpleHttpServe )
import           Snap.Http.Server.Config        ( Config
                                                , defaultConfig
                                                )
import           Friend                         ( Friend
                                                , findFriend
                                                )
import           Paths_snap_friend_list_server

file :: IO FilePath
file = getDataFileName "data/friends.json"


friends :: IO [Friend]
friends = do
  contents <- LBS.readFile =<< file
  let fs = eitherDecode contents
  return $ fromRight [] fs


main :: IO ()
main =
  (simpleHttpServe :: Config Snap a -> Snap () -> IO ()) defaultConfig site

site :: Snap ()
site = ifTop queryHandler

queryHandler :: Snap ()
queryHandler = do
  fs <- liftIO friends
  q  <- getParam "q"
  writeJSON (findFriend q fs)

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON d = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ d
