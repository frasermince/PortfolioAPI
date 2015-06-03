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
import Servant

type Name = Text

type PortfolioAPI =
  "presentation" :> ReqBody `[JSON] Name :> ReqBody '[JSON] [Annotation] :> Post '[JSON] [Presentation]

data File = File
 { path     :: text
 , contents :: text
 } deriving (Eq, Show, Generic)

instance FromJSON File

data Annotation = Annotation
 { file       :: File
 , lineNumber :: Int
 , comment    :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON Annotation

data Presentation = Presentation
 { annotations :: [Annotation]
 , name        :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON Presentation

api :: Proxy PortfolioAPI
api = Proxy

server :: Server PortfolioAPI
server = presentation
  where presentation :: Name -> [Annotation] -> EitherT ServantErr IO Presentation
        presenation name annotations = return (Presentation name annotations)

app :: Application
app = serve api server
