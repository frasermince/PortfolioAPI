module Main where

import Network.Wai.Handler.Warp    (run)
import Database.Persist.Postgresql (runSqlPool)

import Config (defaultConfig, Config(..), Environment(..), setLogger, makePool, presetConfig, lookupSetting)
import Server (app)
import Models (doMigrations)


main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let logger = setLogger env
    runSqlPool doMigrations pool
    cfg <- presetConfig
    run port $ logger $ app cfg

