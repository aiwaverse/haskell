{-# LANGUAGE FlexibleContexts #-}

isInFirstHalf :: (Ord a) => a -> [a] -> Bool
isInFirstHalf el list = el `elem` firstHalf
    where firstHalf = take (length list `div` 2) list
