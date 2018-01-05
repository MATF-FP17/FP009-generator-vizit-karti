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
        vbox <- vBoxNew True 10
        entr <- entryNew
        lbl <- labelNew $ Just l
        boxPackStart vbox lbl PackGrow 0
        boxPackStart vbox entr PackGrow 0
        return $ castToWidget vbox
