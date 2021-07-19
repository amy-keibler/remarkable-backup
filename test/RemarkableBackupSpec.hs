{-# LANGUAGE QuasiQuotes #-}

module RemarkableBackupSpec where

import Data.Aeson
import Data.List (isInfixOf)
import Relude
import RemarkableBackup (Template (..), Templates (..))
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Decoding the templates file" $ do
    it "should succeed in parsing a valid template" $ do
      eitherDecodeStrict validTemplate
        `shouldBe` Right
          ( Templates
              { templates =
                  [ Template
                      { name = "Burndown",
                        filename = "burndown",
                        iconCode = "\\ue9fe",
                        categories =
                          [ "Life/organize"
                          ]
                      }
                  ]
              }
          )
    describe "should fail to decode when additional fields are added" $ do
      it "fails at the top level" $ do
        eitherDecodeStrict topLevelTemplateWithNewField
          `shouldSatisfy` (hasUnknownFields "additionalField")
      it "fails for nested data as well" $ do
        eitherDecodeStrict templateWithNewField
          `shouldSatisfy` (hasUnknownFields "unexpectedField")

hasUnknownFields :: String -> Either String Templates -> Bool
hasUnknownFields unknownField (Left output) = "unknown fields" `isInfixOf` output && unknownField `isInfixOf` output
hasUnknownFields _ (Right _) = False

validTemplate :: ByteString
validTemplate =
  [r|
{
    "templates": [
        {
            "name": "Burndown",
            "filename": "burndown",
            "iconCode": "\ue9fe",
            "categories": [
                "Life/organize"
            ]
        }
    ]
}
|]

topLevelTemplateWithNewField :: ByteString
topLevelTemplateWithNewField =
  [r|
{
    "templates": [
        {
            "name": "Burndown",
            "filename": "burndown",
            "iconCode": "\ue9fe",
            "categories": [
                "Life/organize"
            ]
        }
    ],
    "additionalField": true
}
|]

templateWithNewField :: ByteString
templateWithNewField =
  [r|
{
    "templates": [
        {
            "name": "Burndown",
            "filename": "burndown",
            "iconCode": "\ue9fe",
            "categories": [
                "Life/organize"
            ],
            "unexpectedField": 0
        }
    ]
}
|]
