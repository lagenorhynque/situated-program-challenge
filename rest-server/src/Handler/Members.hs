{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Members where

import           Import

import           Control.Lens
import           Data.Aeson.Lens

getMembersR :: Handler Value
getMembersR = do
    ms <- runDB $ selectList [] [] :: Handler [Entity Member]
    returnJson $ map memberValueWithId ms

postMembersR :: Handler Value
postMembersR = do
    m <- requireJsonBody :: Handler Member
    mid <- runDB $ insert m
    returnJson . memberValueWithId $ Entity mid m

getMemberR :: MemberId -> Handler Value
getMemberR memberId = do
    m <- runDB $ get404 memberId
    returnJson . memberValueWithId $ Entity memberId m

memberValueWithId :: Entity Member -> Value
memberValueWithId (Entity mid m) =
    toJSON m & _Object . at "member-id" ?~ toJSON mid
