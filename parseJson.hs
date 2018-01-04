module parseJson where
    
    data LayoutObjectParent =
        LayoutObjectParent {
            name        :: String,
            type        :: String,
            size        :: Size,
            children    :: [LayoutObjectChild]
        } deriving Show
    
    data Size = 
        Size {
            width   :: Float,
            height  :: Float
        } deriving Show
    
    data LayoutObjectChild = 
            RectLayout [LayoutObjectChild]
        |   TextField
        |   ImageField
        |   Image
        |   Text
        deriving Show
         