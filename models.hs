
{-# LANGUAGE DeriveGeneric #-}
module Models (File(..), Annotation(..), Presentation(..)) where
import Data.Aeson
import GHC.Generics
import Data.Text

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
