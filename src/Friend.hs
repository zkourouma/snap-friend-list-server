{-# LANGUAGE DeriveGeneric #-}
module Friend
  ( Friend
  , findFriend
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.ByteString.Char8          ( ByteString )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text                      ( Text
                                                , isPrefixOf
                                                )

data Friend = Friend { id :: Int, name  :: Text, nickName :: Text }  deriving (Generic, Show, Eq)
instance FromJSON Friend
instance ToJSON Friend

findFriend :: Maybe ByteString -> [Friend] -> [Friend]
findFriend Nothing  fs = fs
findFriend (Just q) fs = filter filterFriend fs
 where
  textQ = decodeUtf8 q
  filterFriend :: Friend -> Bool
  filterFriend (Friend _ n n') =
    (textQ `isPrefixOf` n) || (textQ `isPrefixOf` n')
