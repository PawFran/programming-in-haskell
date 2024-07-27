-- fmaps for Maybe by hand
customFmap1 :: Maybe Int -> Int
customFmap1 Nothing     = 0
customFmap1 (Just x)    = x + 1

customFmap2 :: Maybe Int -> Maybe Int -> Int
customFmap2 Nothing Nothing     = 0
customFmap2 Nothing (Just y)    = y
customFmap2 (Just x) Nothing    = x
customFmap2 (Just x) (Just y)   = x + y

-- print $ customFmap1 Nothing