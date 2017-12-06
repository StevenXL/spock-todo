module EmailSpec where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Email (Email)
import qualified Email
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary, property)

prop_aesonRoundtrip ::
       (ToJSON a, FromJSON a, Show a, Eq a, Arbitrary a) => a -> Bool
prop_aesonRoundtrip val = decode (encode val) == Just val

spec :: Spec
spec = do
    describe "encode" $ do
        it "returns a String when given an Email" $ do
            let email = Email.mk "steven@example.com"
            encode email `shouldBe` "\"steven@example.com\""
    describe "decode" $ do
        it "returns an Email when given a String" $ do
            decode "\"steven@example.com\"" `shouldBe`
                (Just $ Email.mk "steven@example.com")
    describe "encoding and decoding" $ do
        it "roundtrips" $ property $ (prop_aesonRoundtrip :: Email -> Bool)
