{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Server (app) where
import Control.Monad.Trans.Either
-- import Data.List
import Network.Wai
import Servant
import Data.Text
import Models
import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Config                       (Config(..))
import Database.Persist.Postgresql  (insert, selectList, Entity(..), fromSqlKey, (==.))
import Data.Int                     (Int64)

type Name = Text
type AppM = ReaderT Config (EitherT ServantErr IO)

type PortfolioAPI =
  "presentations" :> ReqBody '[JSON] Name :> Post '[JSON] Int64
  -- :<|> "presentations" :> Capture "presentationID" Integer :> Get '[JSON] Presentation




-- presentation = Presentation
--                { annotations = [a1, a2]
--                , name        = "name"
--                }
--   where file = File
--                { path     = "THIS IS A PATH"
--                , contents = "CONTENTS OF A FILE"
--                }
--         a1 = Annotation
--               { file       = file
--              , lineNumber = 5
--              , comment    = "This is a comment"
--              }
--         a2 = Annotation
--              { file       = file
--              , lineNumber = 7
--              , comment    = "This is another comment"
--              }

api :: Proxy PortfolioAPI
api = Proxy

app :: Config -> Application
app cfg = serve api (readerServer cfg)

readerServer :: Config -> Server PortfolioAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg


server :: ServerT PortfolioAPI AppM
server = create -- :<|> show
  where create :: Text -> AppM Int64
        create name = do
          presentation <- runDb $ insert $ Presentation  name
          return $ fromSqlKey  presentation

        -- show :: Integer -> EitherT ServantErr IO Presentation
        -- show presentationId = return presentation
