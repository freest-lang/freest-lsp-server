module Util.FileUtil ( getWordFromFile
                     , getWordFromLine
                     ) where


getWordFromFile :: (Int, Int) -> FilePath -> IO String
getWordFromFile (l, c) fp =
    readFile fp >>= return . getWordFromLine c . (!! l) . lines

getWordFromLine :: Int -> String -> String
getWordFromLine = getWordFromLine' []

getWordFromLine' :: String -> Int -> String -> String
getWordFromLine' acc 0 line = acc ++ takeWhile (not . isSpecialCharacter) line
getWordFromLine' acc _ []   = acc
getWordFromLine' acc i (x:xs)
    | isSpecialCharacter x = getWordFromLine' []           (i-1) xs
    | otherwise            = getWordFromLine' (acc ++ [x]) (i-1) xs

isSpecialCharacter :: Char -> Bool
isSpecialCharacter c = c `elem` [ ':', '='
                                , '?', '!'
                                , '(', ')'
                                , ','
                                , '@'
                                , ' '
                                ] -- HACK: space should not be here