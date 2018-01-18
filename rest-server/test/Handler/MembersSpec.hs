{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MembersSpec (spec) where

import           Database.Persist.Sql       (toSqlKey)
import           TestImport

import           Codec.Binary.UTF8.String   (decodeString)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC


spec :: Spec
spec = withApp $ do

    describe "getMembersR" $
        it "returns members" $ do
            m1id <- runDB $ insert Member { memberFirstName = Just "You"
                                          , memberLastName = Just "Watanabe"
                                          , memberEmail = Just "y.watanabe@uranohoshi.ac.jp"
                                          }
            m2id <- runDB $ insert Member { memberFirstName = Just "Yoshiko"
                                          , memberLastName = Just "Tsushima"
                                          , memberEmail = Just "y.tsushima@uranohoshi.ac.jp"
                                          }
            request $ do
                setMethod "GET"
                setUrl MembersR
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack $ encode
                [ object [ "member-id" .= m1id
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

    describe "postMembersR" $
        it "creates a member" $ do
            let m = Member { memberFirstName = Just "Dia"
                           , memberLastName = Just "Kurosawa"
                           , memberEmail = Just "d.kurosawa@uranohoshi.ac.jp"
                           }
            request $ do
                setMethod "POST"
                setUrl MembersR
                setRequestBody $ encode m
                addRequestHeader ("Accept", "application/json")
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            printBody
            Just (Entity mid _) <- runDB $ selectFirst [MemberFirstName ==. Just "Dia"] []
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "member-id" .= mid
                       , "first-name" .= ("Dia" :: Value)
                       , "last-name" .= ("Kurosawa" :: Value)
                       , "email" .= ("d.kurosawa@uranohoshi.ac.jp" :: Value)
                       ]

    describe "getMemberR" $ do
        it "finds a member" $ do
            mid <- runDB $ insert Member { memberFirstName = Just "Yoshiko"
                                         , memberLastName = Just "Tsushima"
                                         , memberEmail = Just "y.tsushima@uranohoshi.ac.jp"
                                         }
            request $ do
                setMethod "GET"
                setUrl $ MemberR mid
                addRequestHeader ("Accept", "application/json")

            statusIs 200
            printBody
            bodyEquals . decodeString . BLC.unpack . encode $
                object [ "member-id" .= mid
                       , "first-name" .= ("Yoshiko" :: Value)
                       , "last-name" .= ("Tsushima" :: Value)
                       , "email" .= ("y.tsushima@uranohoshi.ac.jp" :: Value)
                       ]

        it "doesn't find a member" $ do
            let mid = toSqlKey 100
            request $ do
                setMethod "GET"
                setUrl $ MemberR mid
                addRequestHeader ("Accept", "application/json")

            statusIs 404
            printBody
