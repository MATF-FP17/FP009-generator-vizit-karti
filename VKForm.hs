module VKForm where
    import Graphics.UI.Gtk
    import VKTemplate

    getFields :: LayoutObject -> [LayoutObject]
    getFields (RectLayout _ _ _ children) = _getFieldsListHelper children
    getFields displayable = if isField displayable then [displayable] else []

    _getFieldsListHelper :: [LayoutObject] -> [LayoutObject]
    _getFieldsListHelper [] = []
    _getFieldsListHelper (x:xs) = getFields x ++ _getFieldsListHelper xs


    _toInput :: LayoutObject -> IO Widget
    _toInput (TextField l _ _ _) = do
        entr <- entryNew
        _wrapLabeledElement entr l
    _toInput (ImageField l _ _) = do
        entr <- fileChooserButtonNew ("Select " ++ l ++ " image") (FileChooserActionOpen)
        _wrapLabeledElement entr l

    _wrapLabeledElement elem l = do
        vbox <- vBoxNew True 10
        lbl <- labelNew $ Just l
        boxPackStart vbox lbl PackGrow 0
        boxPackStart vbox elem PackGrow 0
        return $ castToWidget vbox


    fromField :: LayoutObject -> String -> LayoutObject
    fromField (ImageField _ p s) value = Image {
        position = p, size = s, value = value
    }
    fromField (TextField _ p s f) value = Text {
        position = p, size = s, value = value, fontSize = f
    }
