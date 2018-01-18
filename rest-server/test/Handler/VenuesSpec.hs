{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.VenuesSpec (spec) where

import           Database.Persist.Sql       (toSqlKey)
import           TestImport

import           Codec.Binary.UTF8.String   (decodeString)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Handler.Venues             (PostAddress (..), PostVenue (..))

spec :: Spec
spec = withApp $ do

    describe "getVenuesR" $
        it "returns venues" $ do
            let gid = toSqlKey 1
            v1id <- runDB $ insert Venue { venueName = Just "Yokohama Arena"
                                         , venuePostalCode = Just "222-0033"
                                         , venuePrefecture = Just "Kanagawa-ken"
                                         , venueCity = Just "Yokohama-shi"
                                         , venueStreet1 = Just "Kohoku-ku"
                                         , venueStreet2 = Just "3-10 Shinyokohama"
                                         , venueGroupId = Just gid
                                         }
            v2id <- runDB $ insert Venue { venueName = Just "Nippon Gaishi Hall"
                                         , venuePostalCode = Just "457-0833"
                                         , venuePrefecture = Just "Aichi-ken"
                                         , venueCity = Just "Nagoya-shi"
                                         , venueStreet1 = Just "Minami-ku"
                                         , venueStreet2 = Just "5-1-16 Higashimatabeecho"
                                         , venueGroupId = Just gid
                                         }
            request $ do
                setMethod "GET"
                setUrl $ VenuesR gid
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack $ encode
                [ object [ "venue-id" .= v1id
                         , "venue-name" .= ("Yokohama Arena" :: Value)
                         , "address" .= object [ "postal-code" .= ("222-0033" :: Value)
                                               , "prefecture" .= ("Kanagawa-ken" :: Value)
                                               , "city" .= ("Yokohama-shi" :: Value)
                                               , "address1" .= ("Kohoku-ku" :: Value)
                                               , "address2" .= ("3-10 Shinyokohama" :: Value)
                                               ]
                         ]
                , object [ "venue-id" .= v2id
                         , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                         , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                               , "prefecture" .= ("Aichi-ken" :: Value)
                                               , "city" .= ("Nagoya-shi" :: Value)
                                               , "address1" .= ("Minami-ku" :: Value)
                                               , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                               ]
                         ]
                ]

    describe "postVenuesR" $
        it "creates a venue" $ do
            let gid = toSqlKey 1
                v = PostVenue { postVenueName = Just "Kobe World Memorial Hall"
                              , postAddress = PostAddress { postPostalCode = Just "650-0046"
                                                          , postPrefecture = Just "Hyogo-ken"
                                                          , postCity = Just "Kobe-shi"
                                                          , postAddress1 = Just "Chuo-ku"
                                                          , postAddress2 = Just "6-6-12-2 Minatojima Nakamachi"
                                                          }
                              }
            request $ do
                setMethod "POST"
                setUrl $ VenuesR gid
                setRequestBody $ encode v
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            Just (Entity vid _) <- runDB $ selectFirst [VenueName ==. Just "Kobe World Memorial Hall"] []
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "venue-id" .= vid
                       , "venue-name" .= ("Kobe World Memorial Hall" :: Value)
                       , "address" .= object [ "postal-code" .= ("650-0046" :: Value)
                                             , "prefecture" .= ("Hyogo-ken" :: Value)
                                             , "city" .= ("Kobe-shi" :: Value)
                                             , "address1" .= ("Chuo-ku" :: Value)
                                             , "address2" .= ("6-6-12-2 Minatojima Nakamachi" :: Value)
                                             ]
                       ]
