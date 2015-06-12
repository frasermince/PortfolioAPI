{-# LANGUAGE OverloadedStrings #-}
module ServerSpec (spec) where
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai
import Server
import Config
ioApp :: IO Application
ioApp = do
     config <- presetConfig
     return $ app config
spec :: Spec
spec = with ioApp $
  describe "POST /presentations" $
    it "responds" $
      post "/presentations" "" `shouldRespondWith` 200
  --describe "GET /presentations/:id" $
    --it "responds" $
     -- get "/presentations/1" `shouldRespondWith` 200
