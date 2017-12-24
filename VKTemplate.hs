module VKTemplate where

    type Position = (Float, Float)
    type Size = (Float, Float)
    type Label = String
    type Value = String

    data LayoutPair = LayoutPair Position Size deriving (Show)
    data LayoutObjectType = RectLayout [LayoutObject]
                          | TextField Label 
                          | ImageField Label
                          | Image Value
                          | Text Value Int deriving (Show)
    data LayoutObject = LayoutObject LayoutPair LayoutObjectType deriving (Show)

    class Layoutable e where
        pos :: e -> Position
        size :: e -> Size
        getX :: e -> Float
        getY :: e -> Float
        getWidth :: e -> Float
        getHeight :: e -> Float
        getX l = fst $ pos $ l
        getY l = snd $ pos $ l
        getWidth l = fst $ size $ l
        getHeight l = snd $ size $ l

    instance Layoutable (LayoutPair) where
        pos (LayoutPair p _) = p
        size (LayoutPair _ s) = s

    instance Layoutable (LayoutObject) where
        pos (LayoutObject p _) = pos p
        size (LayoutObject s _) = size s
