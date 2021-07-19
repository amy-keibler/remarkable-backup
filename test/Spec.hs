module Main where

import Test.Hspec
import Relude

import qualified RemarkableBackupSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RemarkableBackup" RemarkableBackupSpec.spec
