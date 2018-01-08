module VKForm where
    import Graphics.UI.Gtk
    import VKTemplate

    getFields :: LayoutObject -> [LayoutObject]
    getFields (LayoutObject _ (RectLayout children)) = _getFieldsListHelper children
    getFields displayable@(LayoutObject _ obj) = if isField obj then [displayable] else []

    _getFieldsListHelper :: [LayoutObject] -> [LayoutObject]
    _getFieldsListHelper [] = []
    _getFieldsListHelper (x:xs) = getFields x ++ _getFieldsListHelper xs


    _toInput :: LayoutObject -> IO Widget
    _toInput (LayoutObject _ (TextField l)) = do
        entr <- entryNew
        _wrapLabeledElement entr l
    _toInput (LayoutObject _ (ImageField l)) = do
        entr <- fileChooserButtonNew ("Select " ++ l ++ " image") (FileChooserActionOpen)
        _wrapLabeledElement entr l

    _wrapLabeledElement elem l = do
        vbox <- vBoxNew True 10
        lbl <- labelNew $ Just l
        boxPackStart vbox lbl PackGrow 0
        boxPackStart vbox elem PackGrow 0
        return $ castToWidget vbox
