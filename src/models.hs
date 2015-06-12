{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
module Models                      (File(..), Annotation(..), Presentation(..), doMigrations, runDb) where
import GHC.Generics
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Data.Text
import Database.Persist.Postgresql (SqlBackend(..), runMigration,
                                    runSqlPool)
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Presentation json
    annotations
    name Text
    deriving Show
Annotation json
    lineNumber Int
    comment Text
    presentationId PresentationId
    deriving Show
File json
    path Text
    contents Text
    annotationId AnnotationId
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

