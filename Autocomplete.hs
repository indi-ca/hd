module Autocomplete (basicComplete) where


import Data.List (elemIndex)


completions = ["motor", "lego"]


basicComplete :: [String] -> String -> Maybe String
basicComplete set sample = case index of Nothing -> Nothing
                                         Just x -> Just (set !! x)
    where lev = map (\x -> startsWith sample x) set
          index = elemIndex True lev

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith a b = and $ zipWith (==) a b


