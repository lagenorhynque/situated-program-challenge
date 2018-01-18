{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MeetupsSpec (spec) where

import           Data.Time.Clock            (UTCTime)
import           Database.Persist.Sql       (toSqlKey)
import           Prelude                    (read)
import           TestImport

import           Codec.Binary.UTF8.String   (decodeString)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC

spec :: Spec
spec = withApp $ do

    describe "getMeetupsR" $
        it "returns meetups" $ do
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
            mu1id <- runDB $ insert Meetup { meetupTitle = "Aqours First LoveLive!"
                                           , meetupStartAt = Just (read "2017-02-25 07:30:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-02-25 12:30:00.000" :: UTCTime)
                                           , meetupVenueId = Just v1id
                                           , meetupGroupId = Just gid
                                           }
            mu2id <- runDB $ insert Meetup { meetupTitle = "Aqours 2nd LoveLive! Nagoya"
                                           , meetupStartAt = Just (read "2017-08-05 08:00:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-08-05 13:00:00.000" :: UTCTime)
                                           , meetupVenueId = Just v2id
                                           , meetupGroupId = Just gid
                                           }
            m1id <- runDB $ insert Member { memberFirstName = Just "You"
                                          , memberLastName = Just "Watanabe"
                                          , memberEmail = Just "y.watanabe@uranohoshi.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Yoshiko"
                                          , memberLastName = Just "Tsushima"
                                          , memberEmail = Just "y.tsushima@uranohoshi.ac.jp"
                                          }
            m3id <- runDB $ insert Member { memberFirstName = Just "Dia"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "d.kurosawa@uranohoshi.ac.jp"
                                          }
            m4id <- runDB $ insert Member { memberFirstName = Just "Ruby"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "r.kurosawa@uranohoshi.ac.jp"
                                          }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m1id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m2id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m3id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m4id
                                             }
            request $ do
                setMethod "GET"
                setUrl $ MeetupsR gid
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack $ encode
                [ object [ "event-id" .= mu1id
                         , "title" .= ("Aqours First LoveLive!" :: Value)
                         , "start-at" .= (read "2017-02-25 07:30:00.000" :: UTCTime)
                         , "end-at" .= (read "2017-02-25 12:30:00.000" :: UTCTime)
                         , "venue" .= object [ "venue-id" .= v1id
                                             , "venue-name" .= ("Yokohama Arena" :: Value)
                                             , "address" .= object [ "postal-code" .= ("222-0033" :: Value)
                                                                   , "prefecture" .= ("Kanagawa-ken" :: Value)
                                                                   , "city" .= ("Yokohama-shi" :: Value)
                                                                   , "address1" .= ("Kohoku-ku" :: Value)
                                                                   , "address2" .= ("3-10 Shinyokohama" :: Value)
                                                                   ]
                                             ]
                         , "members" .= [ object [ "member-id" .= m1id
                                                 , "first-name" .= ("You" :: Value)
                                                 , "last-name" .= ("Watanabe" :: Value)
                                                 , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                                 ]
                                        , object [ "member-id" .= m2id
                                                 , "first-name" .= ("Yoshiko" :: Value)
                                                 , "last-name" .= ("Tsushima" :: Value)
                                                 , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                                                 ]
                                        ]
                         ]
                , object [ "event-id" .= mu2id
                         , "title" .= ("Aqours 2nd LoveLive! Nagoya" :: Value)
                         , "start-at" .= (read "2017-08-05 08:00:00.000" :: UTCTime)
                         , "end-at" .= (read "2017-08-05 13:00:00.000" :: UTCTime)
                         , "venue" .= object [ "venue-id" .= v2id
                                             , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                                             , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                                                   , "prefecture" .= ("Aichi-ken" :: Value)
                                                                   , "city" .= ("Nagoya-shi" :: Value)
                                                                   , "address1" .= ("Minami-ku" :: Value)
                                                                   , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                                                   ]
                                             ]
                         , "members" .= [ object [ "member-id" .= m3id
                                                 , "first-name" .= ("Dia" :: Value)
                                                 , "last-name" .= ("Kurosawa" :: Value)
                                                 , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                                                 ]
                                        , object [ "member-id" .= m4id
                                                 , "first-name" .= ("Ruby" :: Value)
                                                 , "last-name" .= ("Kurosawa" :: Value)
                                                 , "email" .= ("r.kurosawa@uranohoshi.ac.jp" :: Value)
                                                 ]
                                        ]
                         ]
                ]

    describe "postMeetupsR" $
        it "creates a meetup" $ do
            let gid = toSqlKey 1
            vid <- runDB $ insert Venue { venueName = Just "Kobe World Memorial Hall"
                                        , venuePostalCode = Just "650-0046"
                                        , venuePrefecture = Just "Hyogo-ken"
                                        , venueCity = Just "Kobe-shi"
                                        , venueStreet1 = Just "Chuo-ku"
                                        , venueStreet2 = Just "6-6-12-2 Minatojima Nakamachi"
                                        , venueGroupId = Just gid
                                        }
            let mu = Meetup { meetupTitle = "Aqours 2nd LoveLive! Kobe"
                            , meetupStartAt = Just (read "2017-08-19 08:00:00.000" :: UTCTime)
                            , meetupEndAt = Just (read "2017-08-19 13:00:00.000" :: UTCTime)
                            , meetupVenueId = Just vid
                            , meetupGroupId = Nothing
                            }
            request $ do
                setMethod "POST"
                setUrl $ MeetupsR gid
                setRequestBody $ encode mu
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            Just (Entity muid _) <- runDB $ selectFirst [MeetupTitle ==. "Aqours 2nd LoveLive! Kobe"] []
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "event-id" .= muid
                       , "title" .= ("Aqours 2nd LoveLive! Kobe" :: Value)
                       , "start-at" .= (read "2017-08-19 08:00:00.000" :: UTCTime)
                       , "end-at" .= (read "2017-08-19 13:00:00.000" :: UTCTime)
                       , "venue" .= object [ "venue-id" .= vid
                                           , "venue-name" .= ("Kobe World Memorial Hall" :: Value)
                                           , "address" .= object [ "postal-code" .= ("650-0046" :: Value)
                                                                 , "prefecture" .= ("Hyogo-ken" :: Value)
                                                                 , "city" .= ("Kobe-shi" :: Value)
                                                                 , "address1" .= ("Chuo-ku" :: Value)
                                                                 , "address2" .= ("6-6-12-2 Minatojima Nakamachi" :: Value)
                                                                 ]
                                           ]
                       , "members" .= ([] :: [Value])
                       ]

    describe "getMeetupR" $ do
        it "finds a meetup" $ do
            let gid = toSqlKey 1
            vid <- runDB $ insert Venue { venueName = Just "Nippon Gaishi Hall"
                                        , venuePostalCode = Just "457-0833"
                                        , venuePrefecture = Just "Aichi-ken"
                                        , venueCity = Just "Nagoya-shi"
                                        , venueStreet1 = Just "Minami-ku"
                                        , venueStreet2 = Just "5-1-16 Higashimatabeecho"
                                        , venueGroupId = Just gid
                                        }
            muid <- runDB $ insert Meetup { meetupTitle = "Aqours 2nd LoveLive! Nagoya"
                                           , meetupStartAt = Just (read "2017-08-05 08:00:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-08-05 13:00:00.000" :: UTCTime)
                                           , meetupVenueId = Just vid
                                           , meetupGroupId = Just gid
                                           }
            m1id <- runDB $ insert Member { memberFirstName = Just "Dia"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "d.kurosawa@uranohoshi.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Ruby"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "r.kurosawa@uranohoshi.ac.jp"
                                          }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = muid
                                             , meetupMemberMemberId = m1id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = muid
                                             , meetupMemberMemberId = m2id
                                             }
            request $ do
                setMethod "GET"
                setUrl $ MeetupR gid muid
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "event-id" .= muid
                       , "title" .= ("Aqours 2nd LoveLive! Nagoya" :: Value)
                       , "start-at" .= (read "2017-08-05 08:00:00.000" :: UTCTime)
                       , "end-at" .= (read "2017-08-05 13:00:00.000" :: UTCTime)
                       , "venue" .= object [ "venue-id" .= vid
                                           , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                                           , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                                                 , "prefecture" .= ("Aichi-ken" :: Value)
                                                                 , "city" .= ("Nagoya-shi" :: Value)
                                                                 , "address1" .= ("Minami-ku" :: Value)
                                                                 , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                                                 ]
                                           ]
                       , "members" .= [ object [ "member-id" .= m1id
                                               , "first-name" .= ("Dia" :: Value)
                                               , "last-name" .= ("Kurosawa" :: Value)
                                               , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                                               ]
                                      , object [ "member-id" .= m2id
                                               , "first-name" .= ("Ruby" :: Value)
                                               , "last-name" .= ("Kurosawa" :: Value)
                                               , "email" .= ("r.kurosawa@uranohoshi.ac.jp" :: Value)
                                               ]
                                      ]
                       ]

        it "doesn't find a meetup" $ do
            let gid = toSqlKey 1
                muid = toSqlKey 100
            request $ do
                setMethod "GET"
                setUrl $ MeetupR gid muid
                addRequestHeader ("Accept", "application/json")

            statusIs 404
            printBody

    describe "postMeetupMemberR" $
        it "creates a meetup member" $ do
            let gid = toSqlKey 1
            vid <- runDB $ insert Venue { venueName = Just "Nippon Gaishi Hall"
                                        , venuePostalCode = Just "457-0833"
                                        , venuePrefecture = Just "Aichi-ken"
                                        , venueCity = Just "Nagoya-shi"
                                        , venueStreet1 = Just "Minami-ku"
                                        , venueStreet2 = Just "5-1-16 Higashimatabeecho"
                                        , venueGroupId = Just gid
                                        }
            muid <- runDB $ insert Meetup { meetupTitle = "Aqours 2nd LoveLive! Nagoya"
                                           , meetupStartAt = Just (read "2017-08-05 08:00:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-08-05 13:00:00.000" :: UTCTime)
                                           , meetupVenueId = Just vid
                                           , meetupGroupId = Just gid
                                           }
            m1id <- runDB $ insert Member { memberFirstName = Just "You"
                                          , memberLastName = Just "Watanabe"
                                          , memberEmail = Just "y.watanabe@uranohoshi.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Dia"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "d.kurosawa@uranohoshi.ac.jp"
                                          }
            m3id <- runDB $ insert Member { memberFirstName = Just "Ruby"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "r.kurosawa@uranohoshi.ac.jp"
                                          }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = muid
                                             , meetupMemberMemberId = m2id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = muid
                                             , meetupMemberMemberId = m3id
                                             }
            request $ do
                setMethod "POST"
                setUrl $ MeetupMemberR m1id muid
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "event-id" .= muid
                       , "title" .= ("Aqours 2nd LoveLive! Nagoya" :: Value)
                       , "start-at" .= (read "2017-08-05 08:00:00.000" :: UTCTime)
                       , "end-at" .= (read "2017-08-05 13:00:00.000" :: UTCTime)
                       , "venue" .= object [ "venue-id" .= vid
                                           , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                                           , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                                                 , "prefecture" .= ("Aichi-ken" :: Value)
                                                                 , "city" .= ("Nagoya-shi" :: Value)
                                                                 , "address1" .= ("Minami-ku" :: Value)
                                                                 , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                                                 ]
                                           ]
                       , "members" .= [ object [ "member-id" .= m1id
                                               , "first-name" .= ("You" :: Value)
                                               , "last-name" .= ("Watanabe" :: Value)
                                               , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                               ]
                                      , object [ "member-id" .= m2id
                                               , "first-name" .= ("Dia" :: Value)
                                               , "last-name" .= ("Kurosawa" :: Value)
                                               , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                                               ]
                                      , object [ "member-id" .= m3id
                                               , "first-name" .= ("Ruby" :: Value)
                                               , "last-name" .= ("Kurosawa" :: Value)
                                               , "email" .= ("r.kurosawa@uranohoshi.ac.jp" :: Value)
                                               ]
                                      ]
                       ]
