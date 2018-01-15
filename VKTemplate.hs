{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VKTemplate where

    import Data.Aeson
    import Control.Applicative
    import Control.Monad
    import qualified Data.ByteString.Lazy as B
    import GHC.Generics

    instance FromJSON LayoutObject
    instance ToJSON LayoutObject

    instance FromJSON Size
    instance ToJSON Size

    instance FromJSON Position
    instance ToJSON Position

    data LayoutObject = RectLayout {
        name        :: String,
        size        :: Size,
        position    :: Position,
        children    :: [LayoutObject]
    }    |   TextField {
        label       :: String,
        position    :: Position,
        size        :: Size,
        fontsize    :: Double
    }    |   ImageField {
        label       :: String,
        position    :: Position,
        size        :: Size
    }    |   Image {
        position    :: Position,
        size        :: Size,
        value       :: String
    }    |   Text {
        position    :: Position,
        size        :: Size,
        value       :: String,
        fontsize    :: Double
    }    |   MetaLabel {
        message     :: String
    } deriving (Show, Generic)

    isField :: LayoutObject -> Bool
    isField (TextField _ _ _ _) = True
    isField (ImageField _ _ _) = True
    isField _ = False

    data Size =
        Size {
            width   :: Double,
            height  :: Double
        } deriving (Show, Generic)

    data Position =
        Position {
            x :: Double,
            y :: Double
        } deriving (Show, Generic)

    loadTemplate :: FilePath -> IO (Either String LayoutObject)
    loadTemplate v =
        do
            -- Get JSON data and decode it
            d <- (eitherDecode <$> (B.readFile v)) :: IO (Either String LayoutObject)
            -- If d is Left, the JSON was malformed.
            -- In that case, we report the error.
            -- Otherwise, we perform the operation of
            -- our choice. In this case, just print it.
            return d
