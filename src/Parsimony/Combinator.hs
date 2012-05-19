-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Combinator
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- Commonly used generic combinators
--
-----------------------------------------------------------------------------

module Parsimony.Combinator where

import Parsimony.Prim
import Parsimony.Error
import Parsimony.Pos
import Parsimony.Stream
import Control.Applicative hiding (many)


-- | The resulting parser behaves like one of the parsers in the list.
-- The chosen parser is the first one that (i) consumes some input,
-- or (ii) succeeds with a result.
choice             :: [Parser t a] -> Parser t a
choice ps           = foldr (<|>) empty ps

-- | Behaves like the parameter parser, unless it fails without consuming
-- any input.  In that case we succeed with the given value.
option             :: a -> Parser t a -> Parser t a
option x p          = p <|> pure x

skip               :: Parser t a -> Parser t ()
skip p              = (p *> pure ()) <|> return ()

between            :: Parser t open -> Parser t close
                   -> Parser t a -> Parser t a
between o c p       = o *> p <* c

-- | Skip at leats one occurance of input recognized by the parser.
skipMany1          :: Parser t a -> Parser t ()
skipMany1 p         = p *> skipMany p

-- | Apply a parser repeatedly, and collect the results in a list.
many               :: Parser t a -> Parser t [a]
many p              = reverse <$> foldMany (flip (:)) [] p

-- | Apply a parser repeatedly, and collect the results in a list.
-- The resulting list is guaranteed to be at leats of length one.
many1              :: Parser t a -> Parser t [a]
many1 p             = (:) <$> p <*> many p

sepBy              :: Parser t a -> Parser t sep -> Parser t [a]
sepBy p sep         = option [] (sepBy1 p sep)

sepBy1             :: Parser t a -> Parser t sep -> Parser t [a]
sepBy1 p sep        = (:) <$> p <*> many (sep *> p)

endBy1,endBy       :: Parser t  a -> Parser t  sep -> Parser t  [a]
endBy1 p sep        = many1 (p <* sep)
endBy p sep         = many  (p <* sep)

sepEndBy           :: Parser t a -> Parser t sep -> Parser t [a]
sepEndBy p sep      = option [] (sepEndBy1 p sep)

sepEndBy1          :: Parser t a -> Parser t sep -> Parser t [a]
sepEndBy1 p sep     = do x <- p
                         reverse <$> foldManyWhile (flip (:)) [x] loopP
  where
  loopP = option Nothing (sep >> option Nothing (Just <$> p))

-- directly recursive
count              :: Int -> Parser t  a -> Parser t  [a]
count n p           = sequence (replicate n p)


--------------------------------------------------------------------------------

-- | Matches any token.  Fails if there are no more tokens left.
anyToken           :: Stream s t => Parser s t
anyToken            = primParser getToken

-- | Matches the end of the input (i.e., when there are no more tokens
-- to extract).
eof                :: Stream s t => Parser s ()
eof                 = notFollowedBy' showToken anyToken <?> "end of input"

-- | Succeeds if the given parser fails.  The function is used
-- to display the result in error messages.
notFollowedBy'     :: (a -> String) -> Parser t a -> Parser t ()
notFollowedBy' sh p = skip $ do s <- getState
                                a <- p
                                setState s
                                unexpected (sh a)

-- | Succeeds if the given parser fails.
-- Uses the 'Show' instance of the result type in error messages.
notFollowedBy      :: Show a => Parser t a -> Parser t ()
notFollowedBy       = notFollowedBy' show

-- | Parse a list of values recognized by the given parser.
-- The sequence of values should be terminated by a pattern recognized
-- by the terminator patser.
-- The terminator is tried before the value pattern, so if there
-- is overlap between the two, the terminator is recognized.
manyTill :: Parser t a -> Parser t end -> Parser t [a]
manyTill p end = scan
  where scan  =  (end *> return []) <|> ((:) <$> p <*> scan)

getInput           :: Parser t t
getInput            = stateInput <$> getState

setInput           :: t -> Parser t ()
setInput i          = updateState (\s -> s { stateInput = i })

updateInput        :: (t -> t) -> Parser t ()
updateInput f       = updateState (\s -> s { stateInput = f (stateInput s) })

getPosition        :: Parser t SourcePos
getPosition         = statePos <$> getState

setPosition        :: SourcePos -> Parser t ()
setPosition i       = updateState (\s -> s { statePos = i })

updatePosition     :: (SourcePos -> SourcePos) -> Parser t ()
updatePosition f    = updateState (\s -> s { statePos = f (statePos s)})

setState           :: State t -> Parser t ()
setState s          = updateState (\_ -> s)




infix  0 <?>

-- | Specify the name to be used if the given parser fails.
(<?>)              :: Parser t a -> String -> Parser t a
p <?> l             = labels p [l]



unexpected         :: String -> Parser t a
unexpected x        = parseError $ newErrorMessage $ UnExpect x




-- | Apply a parser to the given named input.
parseSource        :: Parser t a    -- ^ The parser to apply
                   -> SourceName    -- ^ A name for the input (used in errors)
                   -> t             -- ^ The input
                   -> Either ParseError a

parseSource p s i   = case runParser p $ State i $ initialPos s of
                        Error err   -> Left err
                        Ok a _      -> Right a

-- | Apply a parser to the given input.
parse              :: Parser t a           -- ^ The parser to apply
                   -> t                    -- ^ The input
                   -> Either ParseError a

parse p             = parseSource p ""







