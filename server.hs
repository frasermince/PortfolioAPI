{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text

type Name = Text

type PortfolioAPI =
  "presentation" :> ReqBody '[JSON] Name :> ReqBody '[JSON] [Annotation] :> Post '[JSON] Presentation
  :<|> "presentation" :> Capture "presentationID" Integer :> Get '[JSON] Presentation

data File = File
 { path     :: Text
 , contents :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON File
instance ToJSON File

data Annotation = Annotation
 { file       :: File
 , lineNumber :: Int
 , comment    :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON Annotation
instance ToJSON Annotation

data Presentation = Presentation
 { annotations :: [Annotation]
 , name        :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON Presentation
instance ToJSON Presentation

presentation = Presentation
               { annotations = [a1, a2]
               , name        = "name"
               }
  where file = File
               { path     = "THIS IS A PATH"
               , contents = "CONTENTS OF A FILE"
               }
        a1 = Annotation
              { file       = file
             , lineNumber = 5
             , comment    = "This is a comment"
             }
        a2 = Annotation
             { file       = file
             , lineNumber = 7
             , comment    = "This is another comment"
             }

api :: Proxy PortfolioAPI
api = Proxy

server :: Server PortfolioAPI
server = create :<|> show
  where create :: Name -> [Annotation] -> EitherT ServantErr IO Presentation
        create name annotations = return Presentation {name = name, annotations = annotations}

        show :: Integer -> EitherT ServantErr IO Presentation
        show id = return presentation

app :: Application
app = serve api server

main = run 8080 app
