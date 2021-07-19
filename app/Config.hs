{-# LANGUAGE RecordWildCards #-}
module Config
  ( Options
  , parseOptions
  ) where

import Options.Applicative
import Relude
import System.Environment.XDG.BaseDir
import System.IO (hPutStrLn, stderr)

import RemarkableBackup (Template)

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
  pure $ Left "TODO: Implement toml parsing"

-- From CLI

lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser = PartialOptions
  <$> lastOption (option str (long "ssh-host"))
  <*> lastOption (option str (long "backup-folder"))
  <*> lastOption (option str (long "templates-folder"))
  <*> pure mempty
