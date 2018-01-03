module VKForm where
    import VKTemplate

    getFields :: LayoutObject -> [LayoutObject]
    getFields (LayoutObject _ (RectLayout children)) = _getFieldsListHelper children
    getFields displayable@(LayoutObject _ obj) = if isField obj then [displayable] else []

    _getFieldsListHelper :: [LayoutObject] -> [LayoutObject]
    _getFieldsListHelper [] = []
    _getFieldsListHelper (x:xs) = getFields x ++ _getFieldsListHelper xs
