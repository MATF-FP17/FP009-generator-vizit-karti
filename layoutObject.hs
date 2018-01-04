{-# LANGUAGE DeriveGeneric #-}

module LayoutObject where

    import Data.Aeson
    import GHC.Generics

    data LayoutObject = RectLayout {
        name        :: String,          --name of the layout object
        t           :: String,          --type of the layout object (can't be named 'type' for some reason)
        size        :: Size,            --object size
        position    :: Position,
        children    :: [LayoutObject]   --list of child objects   
    }    |   TextField {
        t           :: String,
        label       :: String,
        position    :: Position,
        size        :: Size,
        fontSize    :: Int
    }    |   ImageField {
        t           :: String,
        label       :: String,
        position    :: Position,
        size        :: Size
    }    |   Image {
        t           :: String,
        position    :: Position,
        size        :: Size,
        value       :: String
    }    |   Text {
        t           :: String,
        position    :: Position,
        size        :: Size,
        value       :: String
    } deriving (Show, Generic)
        
    
    data Size = 
        Size {
            width   :: Float,
            height  :: Float
        } deriving (Show)
    
    data Position =
        Position {
            x :: Float,
            y :: Float
        } deriving (Show)