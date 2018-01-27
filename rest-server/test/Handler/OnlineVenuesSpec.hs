{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.OnlineVenuesSpec (spec) where

import           Database.Persist.Sql       (toSqlKey)
import           TestImport

import           Codec.Binary.UTF8.String   (decodeString)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified CustomField.VenueType      as VenueType
import           Handler.OnlineVenues       (PostVenue (..))

spec :: Spec
spec = withApp $ do

    describe "getOnlineVenuesR" $
        it "returns online venues" $ do
            let gid = toSqlKey 1
            v1id <- runDB $ insert Venue { venueName = Just "ニコニコ動画"
                                         , venuePostalCode = Nothing
                                         , venuePrefecture = Nothing
                                         , venueCity = Nothing
                                         , venueStreet1 = Nothing
                                         , venueStreet2 = Nothing
                                         , venueGroupId = Just gid
                                         , venueUrl = Just "http://www.nicovideo.jp/"
                                         , venueVenueType = Just VenueType.Online
                                         }
            v2id <- runDB $ insert Venue { venueName = Just "AbemaTV"
                                         , venuePostalCode = Nothing
                                         , venuePrefecture = Nothing
                                         , venueCity = Nothing
                                         , venueStreet1 = Nothing
                                         , venueStreet2 = Nothing
                                         , venueGroupId = Just gid
                                         , venueUrl = Just "https://abema.tv/"
                                         , venueVenueType = Just VenueType.Online
                                         }
            request $ do
                setMethod "GET"
                setUrl $ OnlineVenuesR gid
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack $ encode
                [ object [ "online-venue-id" .= v1id
                         , "venue-name" .= ("ニコニコ動画" :: Value)
                         , "url" .= ("http://www.nicovideo.jp/" :: Value)
                         ]
                , object [ "online-venue-id" .= v2id
                         , "venue-name" .= ("AbemaTV" :: Value)
                         , "url" .= ("https://abema.tv/" :: Value)
                         ]
                ]

    describe "postOnlineVenuesR" $
        it "creates an online venue" $ do
            let gid = toSqlKey 1
                v = PostVenue { postVenueName = Just "LINE LIVE"
                              , postUrl = Just "https://live.line.me/"
                              }
            request $ do
                setMethod "POST"
                setUrl $ OnlineVenuesR gid
                setRequestBody $ encode v
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            Just (Entity vid _) <- runDB $ selectFirst [VenueName ==. Just "LINE LIVE"] []
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "online-venue-id" .= vid
                       , "venue-name" .= ("LINE LIVE" :: Value)
                       , "url" .= ("https://live.line.me/" :: Value)
                       ]
