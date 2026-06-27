module PollingSpec (spec) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.Hspec

import Telegram.Bot.API (Update)
import Telegram.Bot.API.GettingUpdates (UpdateId (..))
import Telegram.Bot.Extra.Polling (nextOffset)

-- | A minimal valid 'Update' whose only populated field is the given
-- @update_id@. The Telegram Bot API requires only @update_id@, so the
-- remaining record fields decode to their defaults.
mkUpdate :: Int -> Update
mkUpdate n = case decode (LBS.pack ("{\"update_id\":" ++ show n ++ "}")) of
    Just u  -> u
    Nothing -> error "mkUpdate: failed to decode minimal Update"

spec :: Spec
spec = describe "Polling.nextOffset" $ do
    it "returns Nothing for an empty page" $ do
        nextOffset [] `shouldBe` Nothing

    it "advances by one from the only update id in a single-element page" $ do
        nextOffset [mkUpdate 5] `shouldBe` Just (UpdateId 6)

    it "advances by one from the highest id of an ascending page" $ do
        nextOffset [mkUpdate 1, mkUpdate 2, mkUpdate 3] `shouldBe` Just (UpdateId 4)

    it "advances by one from the maximum id even when the page is not sorted" $ do
        nextOffset [mkUpdate 20, mkUpdate 10] `shouldBe` Just (UpdateId 21)
