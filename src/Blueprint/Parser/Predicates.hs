module Blueprint.Parser.Predicates (
  isInitial,
  isSpecialInitial,
  isLetter,
  isSubsequent,
  isDigit,
  isHexDigit,
  isExplicitSign,
  isSpecialSubsequent,
  isDotSubsequent,
  isSignSubsequent) where

import qualified Data.Char as C

isInitial :: Char -> Bool
isInitial c = isLetter c
           || isSpecialInitial c

isSpecialInitial :: Char -> Bool
isSpecialInitial c = c `elem` ("!$%&*/:<=>?@^_~" :: String)

isLetter :: Char -> Bool
isLetter c = case C.generalCategory c of
               C.UppercaseLetter       -> True -- Lu
               C.LowercaseLetter       -> True -- Ll
               C.TitlecaseLetter       -> True -- Lt
               C.ModifierLetter        -> True -- Lm
               C.OtherLetter           -> True -- Lo
               C.NonSpacingMark        -> True -- Mn
               C.SpacingCombiningMark  -> True -- Mc
               C.EnclosingMark         -> True -- Me
               C.DecimalNumber         -> True -- Nd
               C.LetterNumber          -> True -- Nl
               C.OtherNumber           -> True -- No
               C.DashPunctuation       -> True -- Pd
               C.ConnectorPunctuation  -> True -- Pc
               C.OtherPunctuation      -> True -- Po
               C.CurrencySymbol        -> True -- Sc
               C.MathSymbol            -> True -- Sm
               C.ModifierSymbol        -> True -- Sk
               C.OtherSymbol           -> True -- So
               C.PrivateUse            -> True -- Co
               _ -> c == '\x200C' || c == '\x200D'

isSubsequent :: Char -> Bool
isSubsequent c = isInitial c
              || isDigit c
              || isSpecialSubsequent c

isDigit :: Char -> Bool
isDigit = C.isDigit

isHexDigit :: Char -> Bool
isHexDigit = C.isHexDigit

isExplicitSign :: Char -> Bool
isExplicitSign '+' = True
isExplicitSign '-' = True
isExplicitSign _ = False

isSpecialSubsequent :: Char -> Bool
isSpecialSubsequent '.' = True
isSpecialSubsequent '@' = True
isSpecialSubsequent c = isExplicitSign c

isDotSubsequent :: Char -> Bool
isDotSubsequent '.' = True
isDotSubsequent c = isSignSubsequent c

isSignSubsequent :: Char -> Bool
isSignSubsequent '@' = True
isSignSubsequent c = isInitial c
                  || isExplicitSign c
