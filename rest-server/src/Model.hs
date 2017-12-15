{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           ClassyPrelude.Yesod
import           Data.Aeson.TH
import           Database.Persist.Quasi

import qualified Data.Aeson.Casing      as Casing
import qualified Text.Casing            as Casing

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriveJSON (Casing.aesonPrefix Casing.kebab) ''Group
deriveJSON (Casing.aesonPrefix Casing.kebab) ''Meetup
deriveJSON (Casing.aesonPrefix Casing.kebab) ''Member
deriveJSON (Casing.aesonPrefix Casing.kebab) ''Venue
