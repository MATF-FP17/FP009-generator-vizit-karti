module VKForm where
    import Data.Maybe
    import Graphics.UI.Gtk
    import VKTemplate

    getFields :: LayoutObject -> [LayoutObject]
    getFields (RectLayout _ _ _ children) = _getFieldsListHelper children
    getFields displayable = if isField displayable then [displayable] else []

    _getFieldsListHelper :: [LayoutObject] -> [LayoutObject]
    _getFieldsListHelper [] = []
    _getFieldsListHelper (x:xs) = getFields x ++ _getFieldsListHelper xs


    _toInput :: LayoutObject -> IO (Widget, Widget)
    _toInput (TextField l _ _ _) = do
        entr <- entryNew
        wrapped <- _wrapLabeledElement entr l
        return (wrapped, (castToWidget entr))
    _toInput (ImageField l _ _) = do
        entr <- fileChooserButtonNew ("Select " ++ l ++ " image") (FileChooserActionOpen)
        wrapped <- _wrapLabeledElement entr l
        return (wrapped, (castToWidget entr))

    _wrapLabeledElement elem l = do
        vbox <- vBoxNew True 10
        lbl <- labelNew $ Just l
        boxPackStart vbox lbl PackGrow 0
        boxPackStart vbox elem PackGrow 0
        return $ castToWidget vbox


    createInputs :: Box -> LayoutObject -> IO [Widget]
    createInputs parent template = do
        entries <- mapM _toInput $ getFields template
        mapM_ ((pack parent) . fst) $ entries
        return $ map snd entries

    pack layout item = boxPackStart layout item PackNatural 0


    getFieldsData :: [Widget] -> IO [String]
    getFieldsData l = mapM getFieldData l


    getFieldData :: Widget -> IO String
    getFieldData widget
        | isA widget gTypeEntry = entryGetText $ castToEntry widget
        | isA widget gTypeFileChooser = do
            s <- fileChooserGetURI $ castToFileChooser widget
            return $ fromMaybe "" s


    fromField :: LayoutObject -> String -> LayoutObject
    fromField (ImageField _ p s) value = Image {
        position = p, size = s, value = value
    }
    fromField (TextField _ p s f) value = Text {
        position = p, size = s, value = value, fontSize = f
    }

    countFieldsInSubtree (RectLayout _ _ _ l) = countFieldsInL l
    countFieldsInSubtree x
        | isField x = 1
        | otherwise = 0
    countFieldsInL (x:xs) = countFieldsInSubtree x + countFieldsInL xs

    flListHelper :: [LayoutObject] -> [String] -> [LayoutObject]
    flListHelper [] _ = []
    flListHelper x [] = x
    flListHelper (x@(RectLayout _ _ _ _):xs) values =
        (_fromFields x values) : flListHelper xs  (drop (countFieldsInSubtree x) values)
    flListHelper (x:xs) values@(v:vs)
        | isField x = fromField x v : flListHelper xs vs
        | otherwise = x : flListHelper xs values

    fromFields :: LayoutObject -> [String] -> LayoutObject
    fromFields lyt values = _fromFields lyt values

    _fromFields :: LayoutObject -> [String] -> LayoutObject
    _fromFields (RectLayout n s p l) values = RectLayout {
        name = n, size = s, position = p, children = flListHelper l values
    }
    _fromFields x (v:_) = fromField x v
    _fromFields x [] = fromField x ""
