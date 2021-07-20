{-# LANGUAGE RecordWildCards #-}
module Config
  ( Options
  , parseOptions
  ) where

import Options.Applicative
import Relude
import System.Environment.XDG.BaseDir
import System.Directory
import System.IO (hPutStrLn, stderr)
import qualified Toml
import Toml.Codec hiding (first)

import RemarkableBackup (Template (..), IconCode(..))

data Options = Options
  { oRemarkableSshHost         :: Text
  , oHostBackupFolder          :: Text
  , oAdditionalTemplatesFolder :: Text
  , oAdditionalTemplates       :: [Template]
  } deriving (Show)

parseOptions :: IO Options
parseOptions = do
  cmdLineOptions <- execParser $ info partialOptionsParser mempty
  configOptions <-  partialOptionsConfigFile >>= logErr
  let combinedOptions =  defaultPartialOptions
                      <> configOptions
                      <> cmdLineOptions
  either die return $ mkOptions combinedOptions

logErr :: (Monoid a) => Either String a -> IO a
logErr (Left e) = do
  hPutStrLn stderr e
  pure mempty
logErr (Right a) = pure a

data PartialOptions = PartialOptions
  { poRemarkableSshHost         :: Last Text
  , poHostBackupFolder          :: Last Text
  , poAdditionalTemplatesFolder :: Last Text
  , poAdditionalTemplates       :: Last [Template]
  } deriving (Show)

instance Semigroup PartialOptions where
  x <> y = PartialOptions
    { poRemarkableSshHost = poRemarkableSshHost x <> poRemarkableSshHost y
    , poHostBackupFolder = poHostBackupFolder x <> poHostBackupFolder y
    , poAdditionalTemplatesFolder = poAdditionalTemplatesFolder x <> poAdditionalTemplatesFolder y
    , poAdditionalTemplates = poAdditionalTemplates x <> poAdditionalTemplates y
    }

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty mempty

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

mkOptions :: PartialOptions -> Either String Options
mkOptions PartialOptions {..} = do
  oRemarkableSshHost <- lastToEither "Missing ssh host" poRemarkableSshHost
  oHostBackupFolder <- lastToEither "Missing backup folder" poHostBackupFolder
  oAdditionalTemplatesFolder <- lastToEither "Missing additional templates folder" poAdditionalTemplatesFolder
  oAdditionalTemplates <- lastToEither "Missing additional templates" poAdditionalTemplates
  return Options {..}

-- Default

defaultPartialOptions :: PartialOptions
defaultPartialOptions = mempty
  { poAdditionalTemplates = pure []
  }

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
  <*> Toml.last Toml.text "additional_templates_folder" .= poAdditionalTemplatesFolder
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
  <*> lastOption (option str (long "templates-folder"))
  <*> pure mempty
