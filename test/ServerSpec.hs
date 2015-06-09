{-# LANGUAGE OverloadedStrings #-}
module ServerSpec (spec) where
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai
import Server
spec :: Spec
spec = with (return app) $
  describe "GET /presentations/:id" $
    it "responds" $
      get "/presentations/1" `shouldRespondWith` 200
