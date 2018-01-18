{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GroupsSpec (spec) where

import           Data.Time.Clock            (UTCTime)
import           Prelude                    (read)
import           TestImport

import           Codec.Binary.UTF8.String   (decodeString)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Handler.Groups             (PostGroup (..),
                                             PostGroupMember (..))

spec :: Spec
spec = withApp $ do

    describe "getGroupsR" $
        it "returns groups" $ do
            g1id <- runDB $ insert Group { groupName = Just "μ's"
                                         , groupCreatedAt = Just (read "2010-06-29 15:00:00.000" :: UTCTime)
                                         }
            g2id <- runDB $ insert Group { groupName = Just "Aqours"
                                         , groupCreatedAt = Just (read "2015-06-29 15:00:00.000" :: UTCTime)
                                         }
            v1id <- runDB $ insert Venue { venueName = Just "Tokyo Dome"
                                         , venuePostalCode = Just "112-0004"
                                         , venuePrefecture = Just "Tokyo-to"
                                         , venueCity = Just "Bunkyo-ku"
                                         , venueStreet1 = Just "1-3-61 Koraku"
                                         , venueStreet2 = Just ""
                                         , venueGroupId = Just g1id
                                         }
            v2id <- runDB $ insert Venue { venueName = Just "Yokohama Arena"
                                         , venuePostalCode = Just "222-0033"
                                         , venuePrefecture = Just "Kanagawa-ken"
                                         , venueCity = Just "Yokohama-shi"
                                         , venueStreet1 = Just "Kohoku-ku"
                                         , venueStreet2 = Just "3-10 Shinyokohama"
                                         , venueGroupId = Just g2id
                                         }
            v3id <- runDB $ insert Venue { venueName = Just "Nippon Gaishi Hall"
                                         , venuePostalCode = Just "457-0833"
                                         , venuePrefecture = Just "Aichi-ken"
                                         , venueCity = Just "Nagoya-shi"
                                         , venueStreet1 = Just "Minami-ku"
                                         , venueStreet2 = Just "5-1-16 Higashimatabeecho"
                                         , venueGroupId = Just g2id
                                         }
            mu1id <- runDB $ insert Meetup { meetupTitle = "μ's Final LoveLive!"
                                           , meetupStartAt = Just (read "2016-03-31 05:00:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2016-03-31 11:00:00.000" :: UTCTime)
                                           , meetupVenueId = Just v1id
                                           , meetupGroupId = Just g1id
                                           }
            mu2id <- runDB $ insert Meetup { meetupTitle = "Aqours First LoveLive!"
                                           , meetupStartAt = Just (read "2017-02-25 07:30:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-02-25 12:30:00.000" :: UTCTime)
                                           , meetupVenueId = Just v2id
                                           , meetupGroupId = Just g2id
                                           }
            mu3id <- runDB $ insert Meetup { meetupTitle = "Aqours 2nd LoveLive! Nagoya"
                                           , meetupStartAt = Just (read "2017-08-05 08:00:00.000" :: UTCTime)
                                           , meetupEndAt = Just (read "2017-08-05 13:00:00.000" :: UTCTime)
                                           , meetupVenueId = Just v3id
                                           , meetupGroupId = Just g2id
                                           }
            m1id <- runDB $ insert Member { memberFirstName = Just "Umi"
                                          , memberLastName = Just "Sonoda"
                                          , memberEmail = Just "u.sonoda@otonokizaka.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Eli"
                                          , memberLastName = Just "Ayase"
                                          , memberEmail = Just "e.ayase@otonokizaka.ac.jp"
                                          }
            m3id <- runDB $ insert Member { memberFirstName = Just "You"
                                          , memberLastName = Just "Watanabe"
                                          , memberEmail = Just "y.watanabe@uranohoshi.ac.jp"
                                          }
            m4id <- runDB $ insert Member { memberFirstName = Just "Yoshiko"
                                          , memberLastName = Just "Tsushima"
                                          , memberEmail = Just "y.tsushima@uranohoshi.ac.jp"
                                          }
            m5id <- runDB $ insert Member { memberFirstName = Just "Maki"
                                          , memberLastName = Just "Nishikino"
                                          , memberEmail = Just "m.nishikino@otonokizaka.ac.jp"
                                          }
            m6id <- runDB $ insert Member { memberFirstName = Just "Dia"
                                          , memberLastName = Just "Kurosawa"
                                          , memberEmail = Just "d.kurosawa@uranohoshi.ac.jp"
                                          }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m1id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m2id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m5id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m3id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m4id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m6id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu3id
                                             , meetupMemberMemberId = m3id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu3id
                                             , meetupMemberMemberId = m4id
                                             }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = g1id
                                            , groupMemberMemberId = m1id
                                            , groupMemberAdmin = Just True
                                            }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = g1id
                                            , groupMemberMemberId = m2id
                                            , groupMemberAdmin = Just True
                                            }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = g2id
                                            , groupMemberMemberId = m3id
                                            , groupMemberAdmin = Just True
                                            }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = g2id
                                            , groupMemberMemberId = m4id
                                            , groupMemberAdmin = Just True
                                            }
            request $ do
                setMethod "GET"
                setUrl GroupsR
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack $ encode
                [ object [ "group-id" .= g1id
                         , "group-name" .= ("μ's" :: Value)
                         , "admin" .= [ object [ "member-id" .= m1id
                                               , "first-name" .= ("Umi" :: Value)
                                               , "last-name" .= ("Sonoda" :: Value)
                                               , "email" .= ("u.sonoda@otonokizaka.ac.jp" :: Value)
                                               ]
                                      , object [ "member-id" .= m2id
                                               , "first-name" .= ("Eli" :: Value)
                                               , "last-name" .= ("Ayase" :: Value)
                                               , "email" .= ("e.ayase@otonokizaka.ac.jp" :: Value)
                                               ]
                                      ]
                         , "venues" .= [ object [ "venue-id" .= v1id
                                                , "venue-name" .= ("Tokyo Dome" :: Value)
                                                , "address" .= object [ "postal-code" .= ("112-0004" :: Value)
                                                                      , "prefecture" .= ("Tokyo-to" :: Value)
                                                                      , "city" .= ("Bunkyo-ku" :: Value)
                                                                      , "address1" .= ("1-3-61 Koraku" :: Value)
                                                                      , "address2" .= ("" :: Value)
                                                                      ]
                                                ]
                                       ]
                         , "meetups" .= [ object [ "event-id" .= mu1id
                                                 , "title" .= ("μ's Final LoveLive!" :: Value)
                                                 , "start-at" .= (read "2016-03-31 05:00:00.000" :: UTCTime)
                                                 , "end-at" .= (read "2016-03-31 11:00:00.000" :: UTCTime)
                                                 , "venue" .= object [ "venue-id" .= v1id
                                                                     , "venue-name" .= ("Tokyo Dome" :: Value)
                                                                     , "address" .= object [ "postal-code" .= ("112-0004" :: Value)
                                                                                           , "prefecture" .= ("Tokyo-to" :: Value)
                                                                                           , "city" .= ("Bunkyo-ku" :: Value)
                                                                                           , "address1" .= ("1-3-61 Koraku" :: Value)
                                                                                           , "address2" .= ("" :: Value)
                                                                                           ]
                                                                     ]
                                                 , "members" .= [ object [ "member-id" .= m1id
                                                                         , "first-name" .= ("Umi" :: Value)
                                                                         , "last-name" .= ("Sonoda" :: Value)
                                                                         , "email" .= ("u.sonoda@otonokizaka.ac.jp" :: Value)
                                                                         ]
                                                                , object [ "member-id" .= m2id
                                                                         , "first-name" .= ("Eli" :: Value)
                                                                         , "last-name" .= ("Ayase" :: Value)
                                                                         , "email" .= ("e.ayase@otonokizaka.ac.jp" :: Value)
                                                                         ]
                                                                , object [ "member-id" .= m5id
                                                                         , "first-name" .= ("Maki" :: Value)
                                                                         , "last-name" .= ("Nishikino" :: Value)
                                                                         , "email" .= ("m.nishikino@otonokizaka.ac.jp" :: Value)
                                                                         ]
                                                                ]
                                                 ]
                                        ]
                         ]
                , object [ "group-id" .= g2id
                         , "group-name" .= ("Aqours" :: Value)
                         , "admin" .= [ object [ "member-id" .= m3id
                                               , "first-name" .= ("You" :: Value)
                                               , "last-name" .= ("Watanabe" :: Value)
                                               , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                               ]
                                      , object [ "member-id" .= m4id
                                               , "first-name" .= ("Yoshiko" :: Value)
                                               , "last-name" .= ("Tsushima" :: Value)
                                               , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                                               ]
                                      ]
                         , "venues" .= [ object [ "venue-id" .= v2id
                                                , "venue-name" .= ("Yokohama Arena" :: Value)
                                                , "address" .= object [ "postal-code" .= ("222-0033" :: Value)
                                                                      , "prefecture" .= ("Kanagawa-ken" :: Value)
                                                                      , "city" .= ("Yokohama-shi" :: Value)
                                                                      , "address1" .= ("Kohoku-ku" :: Value)
                                                                      , "address2" .= ("3-10 Shinyokohama" :: Value)
                                                                      ]
                                                ]
                                       , object [ "venue-id" .= v3id
                                                , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                                                , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                                                      , "prefecture" .= ("Aichi-ken" :: Value)
                                                                      , "city" .= ("Nagoya-shi" :: Value)
                                                                      , "address1" .= ("Minami-ku" :: Value)
                                                                      , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                                                      ]
                                                ]
                                       ]
                         , "meetups" .= [ object [ "event-id" .= mu2id
                                                 , "title" .= ("Aqours First LoveLive!" :: Value)
                                                 , "start-at" .= (read "2017-02-25 07:30:00.000" :: UTCTime)
                                                 , "end-at" .= (read "2017-02-25 12:30:00.000" :: UTCTime)
                                                 , "venue" .= object [ "venue-id" .= v2id
                                                                     , "venue-name" .= ("Yokohama Arena" :: Value)
                                                                     , "address" .= object [ "postal-code" .= ("222-0033" :: Value)
                                                                                           , "prefecture" .= ("Kanagawa-ken" :: Value)
                                                                                           , "city" .= ("Yokohama-shi" :: Value)
                                                                                           , "address1" .= ("Kohoku-ku" :: Value)
                                                                                           , "address2" .= ("3-10 Shinyokohama" :: Value)
                                                                                           ]
                                                                     ]
                                                 , "members" .= [ object [ "member-id" .= m3id
                                                                         , "first-name" .= ("You" :: Value)
                                                                         , "last-name" .= ("Watanabe" :: Value)
                                                                         , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                                                         ]
                                                                , object [ "member-id" .= m4id
                                                                         , "first-name" .= ("Yoshiko" :: Value)
                                                                         , "last-name" .= ("Tsushima" :: Value)
                                                                         , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                                                                         ]
                                                                , object [ "member-id" .= m6id
                                                                         , "first-name" .= ("Dia" :: Value)
                                                                         , "last-name" .= ("Kurosawa" :: Value)
                                                                         , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                                                                         ]
                                                                ]
                                                 ]
                                        , object [ "event-id" .= mu3id
                                                 , "title" .= ("Aqours 2nd LoveLive! Nagoya" :: Value)
                                                 , "start-at" .= (read "2017-08-05 08:00:00.000" :: UTCTime)
                                                 , "end-at" .= (read "2017-08-05 13:00:00.000" :: UTCTime)
                                                 , "venue" .= object [ "venue-id" .= v3id
                                                                     , "venue-name" .= ("Nippon Gaishi Hall" :: Value)
                                                                     , "address" .= object [ "postal-code" .= ("457-0833" :: Value)
                                                                                           , "prefecture" .= ("Aichi-ken" :: Value)
                                                                                           , "city" .= ("Nagoya-shi" :: Value)
                                                                                           , "address1" .= ("Minami-ku" :: Value)
                                                                                           , "address2" .= ("5-1-16 Higashimatabeecho" :: Value)
                                                                                           ]
                                                                     ]
                                                 , "members" .= [ object [ "member-id" .= m3id
                                                                         , "first-name" .= ("You" :: Value)
                                                                         , "last-name" .= ("Watanabe" :: Value)
                                                                         , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                                                         ]
                                                                , object [ "member-id" .= m4id
                                                                         , "first-name" .= ("Yoshiko" :: Value)
                                                                         , "last-name" .= ("Tsushima" :: Value)
                                                                         , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                                                                         ]
                                                                ]
                                                 ]
                                        ]
                         ]
                ]

    describe "postGroupsR" $
        it "creates a group" $ do
            m1id <- runDB $ insert Member { memberFirstName = Just "Sarah"
                                          , memberLastName = Just "Kazuno"
                                          , memberEmail = Just "s.kazuno@hakodate-seisen.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Leah"
                                          , memberLastName = Just "Kazuno"
                                          , memberEmail = Just "l.kazuno@hakodate-seisen.ac.jp"
                                          }
            let g = PostGroup { postGroupName = Just "Saint Snow"
                              , postAdminMemberIds = [m1id, m2id]
                              }
            request $ do
                setMethod "POST"
                setUrl GroupsR
                setRequestBody $ encode g
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            Just (Entity gid _) <- runDB $ selectFirst [GroupName ==. Just "Saint Snow"] []
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "group-id" .= gid
                       , "group-name" .= ("Saint Snow" :: Value)
                       , "admin" .= [ object [ "member-id" .= m1id
                                             , "first-name" .= ("Sarah" :: Value)
                                             , "last-name" .= ("Kazuno" :: Value)
                                             , "email" .= ("s.kazuno@hakodate-seisen.ac.jp" :: Value)
                                             ]
                                    , object [ "member-id" .= m2id
                                             , "first-name" .= ("Leah" :: Value)
                                             , "last-name" .= ("Kazuno" :: Value)
                                             , "email" .= ("l.kazuno@hakodate-seisen.ac.jp" :: Value)
                                             ]
                                    ]
                       ]
    describe "postGroupMemberR" $
        it "create a group member" $ do
            gid <- runDB $ insert Group { groupName = Just "Aqours"
                                         , groupCreatedAt = Just (read "2015-06-29 15:00:00.000" :: UTCTime)
                                         }
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
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m1id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m2id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu1id
                                             , meetupMemberMemberId = m3id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m1id
                                             }
            _ <- runDB $ insert MeetupMember { meetupMemberMeetupId = mu2id
                                             , meetupMemberMemberId = m2id
                                             }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = gid
                                            , groupMemberMemberId = m1id
                                            , groupMemberAdmin = Just True
                                            }
            _ <- runDB $ insert GroupMember { groupMemberGroupId = gid
                                            , groupMemberMemberId = m2id
                                            , groupMemberAdmin = Just True
                                            }
            let gm = PostGroupMember { postAdmin = Just True }
            request $ do
                setMethod "POST"
                setUrl $ GroupMemberR m3id gid
                setRequestBody $ encode gm
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "group-id" .= gid
                       , "group-name" .= ("Aqours" :: Value)
                       , "admin" .= [ object [ "member-id" .= m1id
                                             , "first-name" .= ("You" :: Value)
                                             , "last-name" .= ("Watanabe" :: Value)
                                             , "email" .= ("y.watanabe@uranohoshi.ac.jp" :: Value)
                                             ]
                                    , object [ "member-id" .= m2id
                                             , "first-name" .= ("Yoshiko" :: Value)
                                             , "last-name" .= ("Tsushima" :: Value)
                                             , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                                             ]
                                    , object [ "member-id" .= m3id
                                             , "first-name" .= ("Dia" :: Value)
                                             , "last-name" .= ("Kurosawa" :: Value)
                                             , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                                             ]
                                    ]
                       , "venues" .= [ object [ "venue-id" .= v1id
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
                       , "meetups" .= [ object [ "event-id" .= mu1id
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
                                                              , object [ "member-id" .= m3id
                                                                       , "first-name" .= ("Dia" :: Value)
                                                                       , "last-name" .= ("Kurosawa" :: Value)
                                                                       , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
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
                                      ]
                       ]
