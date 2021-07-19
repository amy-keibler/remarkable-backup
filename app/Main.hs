module Main where

import Relude
import qualified Data.Text.IO as TIO

import RemarkableBackup
import Config

main :: IO ()
main = parseOptions >>= print
