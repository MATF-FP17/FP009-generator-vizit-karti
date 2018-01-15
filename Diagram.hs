{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagram where

    import Diagrams.Prelude
    import Diagrams.Backend.Rasterific
    import VKTemplate
    import Data.Either
    import System.IO.Unsafe

    _myDiagramHelper :: [LayoutObject] -> Diagram B
    _myDiagramHelper [x] = myDiagram x
    _myDiagramHelper (x:xs) = myDiagram x `atop` _myDiagramHelper xs


    myDiagram :: LayoutObject -> Diagram B
    myDiagram (RectLayout _ (Size width height) (Position x y) children)  =
        (_myDiagramHelper children)
    myDiagram (TextField _ (Position x y) (Size width height) _) =
        square 1
        # scaleX width
        # scaleY height
        # lwG 0.05
        # translateX x
        # translateY y
    myDiagram (ImageField _ (Position x y) (Size width height)) =
        square 1
        #fc white
        # scaleX width
        # scaleY height
        # lwG 0.05
        # translateX x
        # translateY y
    myDiagram (Image (Position x y) (Size width height) value) =
        (unsafePerformIO $ getImage $ drop 7 value)
        # scaleToX width
        # scaleToY height
        # translateX x
        # translateY y
    myDiagram (Text (Position x y) (Size width height) value fontsize) =
        text value
        # fontSize (normalized fontsize)
        # translateX (1000 * x)
        # translateY (1000 * y)
        # lwG 1
        # fc black
    myDiagram x =
        square 1

    getImage :: String -> IO (Diagram B)
    getImage path = do
        im <- loadImageEmb path
        return $ case im of
            Right r -> image r
            Left _ -> square 1

    mainDiagram template =
        let
            tempW = VKTemplate.width (VKTemplate.size template)
            tempH = VKTemplate.height (VKTemplate.size template)
        in
        renderRasterific "diagram.png"
            (dims2D tempW tempH)
            (myDiagram template `atop` unitSquare # scaleX tempW # scaleY tempH # fc pink)
