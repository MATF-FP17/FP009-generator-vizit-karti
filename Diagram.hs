{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import VKTemplate 

_myDiagramHelper :: [LayoutObject] -> Diagram B
_myDiagramHelper [x] = myDiagram x
_myDiagramHelper (x:xs) =   myDiagram x |||
                            strutX 5 |||
                            _myDiagramHelper xs

myDiagram :: LayoutObject -> Diagram B
myDiagram (RectLayout _ (Size width height) (Position x y) children)  = 
    (_myDiagramHelper children) 
    `atop` square 1
    #fc white
    # scaleX width
    # scaleY height
    # lwG 0.05
    # translateX x
    # translateY y
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
myDiagram (Image (Position x y) (Size width height) _) =
    square 1
    #fc white
    # scaleX width
    # scaleY height
    # lwG 0.05
    # translateX x
    # translateY y
myDiagram (Text (Position x y) (Size width height) value fontsize) =
    text value
    # fontSize (local fontsize) `atop`
    square 1 
    #fc white
    # scaleX width
    # scaleY height
    # lwG 0.05
    # translateX x
    # translateY y
myDiagram x = 
    square 1 

mainDiagram template =
    renderSVG "diagram.svg" 
        (dims2D (VKTemplate.width (VKTemplate.size template)) 
        (VKTemplate.height (VKTemplate.size template))) 
        (myDiagram template)