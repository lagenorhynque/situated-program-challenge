{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Venues where

import           Import

import           Data.Aeson.TH

import qualified Data.Aeson.Casing     as Casing
import qualified Text.Casing           as Casing

import qualified CustomField.VenueType as VenueType

getVenuesR :: GroupId -> Handler Value
getVenuesR groupId = do
    vs <- runDB $ selectList [VenueGroupId ==. Just groupId, VenueVenueType ==. Just VenueType.Physical] []
    returnJson $ map venueValueWithAddress vs

postVenuesR :: GroupId -> Handler Value
postVenuesR groupId = do
    PostVenue{..} <- requireJsonBody :: Handler PostVenue
    let PostAddress{..} = postAddress
        v = Venue { venueName = postVenueName
                  , venuePostalCode = postPostalCode
                  , venuePrefecture = postPrefecture
                  , venueCity = postCity
                  , venueStreet1 = postAddress1
                  , venueStreet2 = postAddress2
                  , venueGroupId = Just groupId
                  , venueUrl = Nothing
                  , venueVenueType = Just VenueType.Physical
                  }
    vid <- runDB $ insert v
    returnJson . venueValueWithAddress $ Entity vid v

venueValueWithAddress :: Entity Venue -> Value
venueValueWithAddress (Entity vid Venue{..}) =
    object [ "venue-id" .= vid
           , "venue-name" .= venueName
           , "address" .= object [ "postal-code" .= venuePostalCode
                                 , "prefecture" .= venuePrefecture
                                 , "city" .= venueCity
                                 , "address1" .= venueStreet1
                                 , "address2" .= venueStreet2
                                 ]
           ]

data PostVenue = PostVenue
    { postVenueName :: Maybe Text
    , postAddress   :: PostAddress
    }

data PostAddress = PostAddress
    { postPostalCode :: Maybe Text
    , postPrefecture :: Maybe Text
    , postCity       :: Maybe Text
    , postAddress1   :: Maybe Text
    , postAddress2   :: Maybe Text
    }

deriveJSON (Casing.aesonPrefix Casing.kebab) ''PostVenue
deriveJSON (Casing.aesonPrefix Casing.kebab) ''PostAddress
