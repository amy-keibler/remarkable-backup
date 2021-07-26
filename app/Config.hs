{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs    #-}
module Config
  ( Options
  , parseOptions
  ) where

import Data.Validation
import Options.Applicative hiding (Failure, Success)
import Relude
import System.Environment.XDG.BaseDir
import System.Directory
import System.IO (hPutStrLn, stderr)
import qualified Toml
import Toml.Codec hiding (first)
import qualified Text.Show
import Data.Text as T hiding (intersperse)

import RemarkableBackup (Template (..), IconCode(..))

data Options = Options
  { oRemarkableSshHost :: Text
  , oHostBackupFolder  :: Text
  , oTemplateOptions   :: Maybe TemplateOptions
  } deriving (Show)

data TemplateOptions = TemplateOptions
  { oAdditionalTemplatesFolder :: Text
  , oAdditionalTemplates       :: [Template]
  } deriving (Show)

data OptionsError
  = MissingSshHost
  | MissingBackupFolder
  | InvalidBackupFolder Text
  | MissingTemplatesFolder
  | InvalidTemplatesFolder Text
  | InvalidTomlConfigFile Text

instance Show OptionsError where
  show :: OptionsError -> String
  show MissingSshHost = "Missing SSH Host"
  show MissingBackupFolder = "Missing Backup Folder"
  show (InvalidBackupFolder f) = "Backup folder " <> T.unpack f <> " is invalid"
  show MissingTemplatesFolder = "Missing templates folder when templates are configured"
  show (InvalidTemplatesFolder f) = "Templates folder " <> T.unpack f <> " is invalid"
  show (InvalidTomlConfigFile f) = "Toml configuration file " <> T.unpack f <> " is invalid"

type OptionsValidation a = Validation [OptionsError] a

parseOptions :: IO (OptionsValidation Options)
parseOptions = do
  cmdLineOptions <- execParser $ info partialOptionsParser mempty
  configOptions <-  partialOptionsConfigFile >>= logErr
  let combinedOptions =  defaultPartialOptions
                      <> configOptions
                      <> cmdLineOptions
  pure $ mkOptions combinedOptions

logErr :: (Monoid a) => Either String a -> IO a
logErr (Left e) = do
  hPutStrLn stderr e
  pure mempty
logErr (Right a) = pure a

data PartialOptions = PartialOptions
  { poRemarkableSshHost :: Last Text
  , poHostBackupFolder  :: Last Text
  , poTemplateOptions    :: Last (Maybe PartialTemplateOptions)
  } deriving (Show)

data PartialTemplateOptions = PartialTemplateOptions
  { poAdditionalTemplatesFolder :: Last Text
  , poAdditionalTemplates       :: Last [Template]
  } deriving (Show)

instance Semigroup PartialOptions where
  x <> y = PartialOptions
    { poRemarkableSshHost = poRemarkableSshHost x <> poRemarkableSshHost y
    , poHostBackupFolder = poHostBackupFolder x <> poHostBackupFolder y
    , poTemplateOptions = poTemplateOptions x <> poTemplateOptions y
    }

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty

instance Semigroup PartialTemplateOptions where
  x <> y = PartialTemplateOptions
    { poAdditionalTemplatesFolder = poAdditionalTemplatesFolder x <> poAdditionalTemplatesFolder y
    , poAdditionalTemplates = poAdditionalTemplates x <> poAdditionalTemplates y
    }

instance Monoid PartialTemplateOptions where
  mempty = PartialTemplateOptions mempty mempty

lastToValidation :: OptionsError -> Last a -> OptionsValidation a
lastToValidation errMsg (Last x) = maybe (Failure [errMsg]) Success x

lastToEmpty :: (Monoid a) => Last a -> a
lastToEmpty (Last (Just x)) = x
lastToEmpty (Last Nothing) = mempty

mkOptions :: PartialOptions -> OptionsValidation Options
mkOptions PartialOptions {..} = Options
  <$> lastToValidation MissingSshHost poRemarkableSshHost
  <*> lastToValidation MissingBackupFolder poHostBackupFolder
  <*> (traverse mkTemplateOptions $ lastToEmpty poTemplateOptions)

mkTemplateOptions :: PartialTemplateOptions -> OptionsValidation TemplateOptions
mkTemplateOptions PartialTemplateOptions {..} = TemplateOptions
  <$> lastToValidation MissingTemplatesFolder poAdditionalTemplatesFolder
  <*> pure (lastToEmpty poAdditionalTemplates)

-- Default

defaultPartialOptions :: PartialOptions
defaultPartialOptions = mempty

-- Config File

partialOptionsConfigFile :: IO (Either String PartialOptions)
partialOptionsConfigFile = do
  configDir <- getUserConfigDir "remarkable-backup"
  let configFile = configDir <> "/config.toml"
  exists <- doesFileExist configFile
  if exists
    then do
      decodedFile <- decodeFileEither partialOptionsCodec configFile
      pure $ first (\e -> mconcat $ intersperse "\n" $ show <$> e) decodedFile
    else
      pure $ Right mempty

partialOptionsCodec :: TomlCodec PartialOptions
partialOptionsCodec = PartialOptions
  <$> Toml.last Toml.text "ssh_host" .= poRemarkableSshHost
  <*> Toml.last Toml.text "backup_folder" .= poHostBackupFolder
  <*> Toml.last (Toml.dioptional . Toml.table templatesSectionCodec) "templates" .= poTemplateOptions

templatesSectionCodec :: TomlCodec PartialTemplateOptions
templatesSectionCodec = PartialTemplateOptions
  <$> Toml.last Toml.text "templates_folder" .= poAdditionalTemplatesFolder
  <*> Toml.last (Toml.list  templatesCodec) "additional_templates" .= poAdditionalTemplates

templatesCodec :: TomlCodec Template
templatesCodec = Template
  <$> Toml.text "name" .= name
  <*> Toml.text "file_name" .= filename
  <*> Toml.diwrap (Toml.text "icon_code") .= iconCode
  <*> Toml.arrayOf _Text "categories" .= categories

-- From CLI

lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser = PartialOptions
  <$> lastOption (option str (long "ssh-host"))
  <*> lastOption (option str (long "backup-folder"))
  <*> pure mempty
