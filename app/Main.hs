{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where


import           GHC.Generics
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , encode
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import           Data.Either                    ( fromRight )
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

data Friend = Friend { id :: Int, name  :: Text, nickName :: Text }  deriving (Generic, Show, Eq)
instance FromJSON Friend
instance ToJSON Friend

friends :: IO [Friend]
friends = do
  contents <- BS.readFile =<< file
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
  maybe (writeJSON fs) writeBS q

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON d = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ d
