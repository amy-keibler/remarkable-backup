{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RemarkableBackup
  ( Templates (..),
    Template (..),
    IconCode (..),
  )
where

import Data.Aeson
import Relude

data Templates = Templates
  { templates :: [Template]
  }
  deriving (Eq, Show, Generic)

data Template = Template
  { name :: Text,
    filename :: Text,
    iconCode :: IconCode,
    categories :: [Text]
  }
  deriving (Eq, Show, Generic)

newtype IconCode = IconCode Text deriving (Eq, Show, Generic, IsString, FromJSON)

customOptions :: Options
customOptions =
  defaultOptions
    { rejectUnknownFields = True
    }

instance FromJSON Templates where
  parseJSON = genericParseJSON customOptions

instance FromJSON Template where
  parseJSON = genericParseJSON customOptions
