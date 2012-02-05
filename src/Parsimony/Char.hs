{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Char
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- Commonly used character parsers.
--
-----------------------------------------------------------------------------

module Parsimony.Char
  ( spaces, space
  , newline, tab
  , upper, lower, alphaNum, letter
  , digit, hexDigit, octDigit
  , char, string
  , anyChar, oneOf, noneOf
  , satisfy
  ) where

import Data.Char
import Parsimony.Prim
import Parsimony.Combinator
import Parsimony.Stream


-----------------------------------------------------------
-- Character parsers
-----------------------------------------------------------
oneOf, noneOf      :: Stream s Char => [Char] -> Parser s Char
oneOf cs            = satisfy (\c -> elem c cs)
noneOf cs           = satisfy (\c -> not (elem c cs))

spaces             :: Stream s Char => Parser s ()
spaces              = skipMany space        <?> "white space"

space              :: Stream s Char => Parser s Char
space               = satisfy (isSpace)     <?> "space"

newline, tab       :: Stream s Char => Parser s ()
newline             = char '\n'             <?> "new-line"
tab                 = char '\t'             <?> "tab"

upper, lower, alphaNum, letter, digit, hexDigit, octDigit
                   :: Stream s Char => Parser s Char
upper               = satisfy (isUpper)     <?> "uppercase letter"
lower               = satisfy (isLower)     <?> "lowercase letter"
alphaNum            = satisfy (isAlphaNum)  <?> "letter or digit"
letter              = satisfy (isAlpha)     <?> "letter"
digit               = satisfy (isDigit)     <?> "digit"
hexDigit            = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit            = satisfy (isOctDigit)  <?> "octal digit"

char               :: Stream s Char => Char -> Parser s ()
char c              = (satisfy (==c) >> return ())  <?> show [c]

anyChar            :: Stream s Char => Parser s Char
anyChar             = anyToken <?> "a character"

satisfy            :: Stream s Char => (Char -> Bool) -> Parser s Char
satisfy f           = (try $ anyChar >>= \c ->
                              if f c then return c
                                     else unexpected (show c))
                        <?> ""

string             :: Stream s Char => String -> Parser s ()
string []           = return ()
string (c:cs)       = try (char c) >> match show cs anyChar




