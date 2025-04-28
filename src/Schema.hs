{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema where

import Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject)
import Database.Persist (Entity(..))
import qualified Database.Persist.TH as PTH
import Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
User sql=users
  title Text
  description Text
  deriving Show Read
|]

instance ToJSON Todo where
  toJSON Todo{..} = object 
    [ "title" .= todoTitle
    , "description" .= todoDescription
    ]

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \o -> do
    title <- o .: "title"
    description <- o .: "description"
    return User
      { todoTitle = title
      , todoDescription = description
      }
