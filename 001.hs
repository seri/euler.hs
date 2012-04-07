main = print $ sum . filter ((==0) . flip . rem 3) $ [1..999]
