{-# LANGUAGE OverloadedStrings #-}
module CustomField.VenueType where

import qualified Data.ByteString.Char8 as BC
import           Database.Persist.Sql

data VenueType = Physical | Online deriving (Eq, Ord, Enum, Bounded)

instance Show VenueType where
    show Physical = "physical"
    show Online   = "online"

instance Read VenueType where
    readsPrec _ "physical" = [(Physical, [])]
    readsPrec _ "online"   = [(Online, [])]
    readsPrec _ _          = []

instance PersistField VenueType where
    toPersistValue = PersistDbSpecific . BC.pack . show
    fromPersistValue (PersistDbSpecific bs) = Right . read $ BC.unpack bs
    fromPersistValue _                      = Left "Not PersistDBSpecific"

instance PersistFieldSql VenueType where
    sqlType _ = SqlOther "venue_type"
