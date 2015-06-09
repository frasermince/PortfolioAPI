{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (app) where
import Control.Monad.Trans.Either
import Data.List
import Network.Wai
import Servant
import Data.Text
import Models

type Name = Text

type PortfolioAPI =
  "presentations" :> ReqBody '[JSON] Name :> ReqBody '[JSON] [Annotation] :> Post '[JSON] Presentation
  :<|> "presentations" :> Capture "presentationID" Integer :> Get '[JSON] Presentation




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
        show presentationId = return presentation

app :: Application
app = serve api server