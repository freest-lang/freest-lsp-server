{-# LANGUAGE OverloadedStrings #-}

module LSP.Util 
    ( buildDiagnostic 
    , originRange
    , originPosition
    , isPositionInsideRange
    , isRangeInsideRange
    )
    where

-- LSP
import           Language.LSP.Types

-- Other
import           Data.Text (pack)

originRange :: Range
originRange = Range originPosition originPosition

originPosition :: Position
originPosition = Position 0 0

isPositionInsideRange :: Position -> Range -> Bool
isPositionInsideRange
    (Position l c)
    (Range (Position l1  c1 ) (Position l2  c2 )) =
        l >= l1 && l <= l2 && c >= c1 && c <=c2

isRangeInsideRange :: Range -> Range -> Bool
isRangeInsideRange 
    (Range (Position l1  c1 ) (Position l2  c2 )) 
    (Range (Position l1' c1') (Position l2' c2')) =
        l1 >= l1' && l2 <= l2' && c1 >= c1' && c2 <= c2'

buildDiagnostic :: String -> Range -> Diagnostic
buildDiagnostic msg range =
    Diagnostic
        range               -- range
        (Just DsError )     -- severity
        Nothing             -- code
        (Just "freest-lsp") -- source
        (pack msg)          -- diagnostic message
        Nothing             -- tags
        (Just (List []))