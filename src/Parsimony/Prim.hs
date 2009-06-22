-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Prim
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The primitive parser combinators.
--
-----------------------------------------------------------------------------

module Parsimony.Prim
  ( Parser, PrimParser
  , runParser, primParser
  , parseError, try, lookAhead, labels
  , foldMany, skipMany, match
  , State(..), getState, updateState, mapState
  ) where

import Parsimony.Pos
import Parsimony.Error
import Control.Applicative(Applicative(..),Alternative(..))
import Control.Monad(liftM,ap,MonadPlus(..))


-- | A parser constructing values of type 'a', with an input
-- buffer of type 't'.
data Parser t a       = P { unP :: State t -> R t a }

-- NOTE: The order of the fields is important!
-- In the rest of the module we use the fact that pattern matching
-- happens left to right to ensure that if matching on the 'Bool'
-- fails, then we will not look at the 'Either' field.
data R t a            = R !Bool (Either ParseError (a, State t))

-- | The parser state.
data State t          = State { stateInput :: !t          -- ^ Token source
                              , statePos   :: !SourcePos  -- ^ Current position
                              }

type PrimParser t a   = State t -> Either ParseError (a,State t)

-- | Define a primitive parser.
-- Consumes input on success.
{-# INLINE primParser #-}
primParser           :: PrimParser t a -> Parser t a
primParser prim       = P $ \s -> case prim s of
                                    r@(Left _) -> R False r
                                    r          -> R True r


{-# INLINE runParser #-}
-- | Convert a parser into a 'PrimParser'.
runParser            :: Parser t a -> PrimParser t a
runParser p s         = case unP p s of
                          R _ x -> x

-- | Access the current parser state.
-- Does not consume input.
{-# INLINE getState #-}
getState             :: Parser t (State t)
getState              = P $ \s -> R False $ Right (s,s)

-- | Modify the current parser state.
-- Returns the old state.
-- Does not consume input.
{-# INLINE updateState #-}
updateState          :: (State s -> State s) -> Parser s ()
updateState f         = P $ \s -> R False $ Right ((),f s)

-- | Change the input stream of a parser.
-- This is useful for extending the input stream with extra information.
-- The first function splits the extended state into a state
-- suitable for use by the given parser and some additional information.
-- The second function combines the extra infomration of the original
-- state with the new partial state, to compute a new extended state.
{-# INLINE mapState #-}
mapState            :: (State big -> (State small,extra))
                    -> (State small -> extra -> State big)
                    -> Parser small a -> Parser big a
mapState extract inject p  = P $ \big ->
  case extract big of
    (small,extra) ->
      case unP p small of
        R c r -> R c $ case r of
                        Left err          -> Left err
                        Right (a,small1)  -> Right (a,inject small1 extra)
    

-- | Fail with the given parser error without consuming any input.
-- The error is applied to the current source position.
{-# INLINE parseError #-}
parseError          :: (SourcePos -> ParseError) -> Parser t a
parseError e         = P $ \s -> R False $ Left $ e $ statePos s




-- | Allow a parser to back-track.  The resulting parser behaves like
-- the input parser unless it fails.  In that case,  we backtrack
-- without consuming any input.  Because we may have to back-track,
-- we keep a hold of the parser input so over-use of this function
-- may result in memory leaks.

{-# INLINE try #-}
try                :: Parser t a -> Parser t a
try p               = P $ \s ->
  case unP p s of
    R True (Left err) -> R False $ Left $ setErrorPos (statePos s) err
    other             -> other


-- | Applies the given parser without consuming any input.
{-# INLINE lookAhead #-}
lookAhead          :: Parser t a -> Parser t a
lookAhead p         = P $ \s ->
  R False $ case unP p s of
              R _ (Left err)    -> Left err
              R _ (Right (a,_)) -> Right (a,s)

-- | The resulting parser behaves like the input parser,
-- except that in case of failure we use the given expectation
-- messages.
{-# INLINE labels #-}
labels             :: Parser t a -> [String] -> Parser t a
labels p msgs0      = P $ \s ->
  case unP p s of
    R c r -> R c (addErr r)

  where setExpectErrors err []         = setErrorMessage (Expect "") err
        setExpectErrors err [msg]      = setErrorMessage (Expect msg) err
        setExpectErrors err (msg:msgs) =
          foldr (\m e -> addErrorMessage (Expect m) e)
             (setErrorMessage (Expect msg) err) msgs

        addErr (Left e) = Left $ setExpectErrors e msgs0
        addErr r        = r


-- | Apply a parser repeatedly, combining the results with the
-- given functions.  This function is similar to the strict 'foldl'.
-- We stop when an application of the parser fails without consuming any
-- input.  If the parser fails after it has consumed some input, then
-- the repeated parser will also fail.

{-# INLINE foldMany #-}
foldMany :: (b -> a -> b) -> b -> Parser t a -> Parser t b
foldMany cons nil p = P $ \s ->
  case unP p s of
    R False (Right _)       -> crash "Parsec.pFold"
    R False (Left _)        -> R False $ Right (nil,s)
    R True  (Right (x,s1))  -> R True  $ (walk $! cons nil x) s1
    R True  (Left err)      -> R True  $ Left err

  -- NOTE: this is written like this because after the first iteration
  -- we already know weather the parser will be consuming input.
  where
  walk xs s =
    case unP p s of
      R False (Right _)       -> crash "Parsec.pFold"
      R False (Left _)        -> Right (xs,s)
      R True  (Right (x,s1))  -> (walk $! cons xs x) s1
      R True  (Left e)        -> Left e


-- | Apply a parser repeatedly, ignoring the results.
-- We stop when an application of the parser fails without consuming any
-- input.  If the parser fails after it has consumed some input, then
-- the repeated parser will also fail.

{-# INLINE skipMany #-}
skipMany :: Parser t a -> Parser t ()
skipMany p = P $ \s ->
  -- pFold specialized for a common case

  case unP p s of
    R False (Right _)       -> crash "Parsec.skipMany"
    R False (Left _)        -> R False $ Right ((),s)
    R True  (Right (_,s1))  -> R True  $ walk s1
    R True  (Left err)      -> R True  $ Left err

  -- NOTE: this is written like this because after the first iteration
  -- we already know weather the parser will be consuming input.
  where
  walk s =
    case unP p s of
      R False (Right _)       -> crash "Parsec.skipMany"
      R False (Left _)        -> Right ((),s)
      R True  (Right (_,s1))  -> walk s1
      R True  (Left e)        -> Left e


-- | Produces a parser that succeeds if it can extract the list of values
-- specified by the list.
-- The function argument specifies how to show the expectations in
-- error messages.
match :: (Eq a) => (a -> String) -> [a] -> Parser t a -> Parser t ()
match sh goal p = P (outer goal)

  where
  expected x          = addErrorMessage (Expect (sh x))
  unexpected x pos    = newErrorMessage (UnExpect (sh x)) pos

  -- not yet consumed
  outer [] s      = R False $ Right ((),s)
  outer (x:xs) s  =
     case unP (labels p [sh x]) s of
       R False (Right (a,s1))
         | x == a    -> outer xs s1
         | otherwise -> R False $ Left $ expected x $ unexpected a $ statePos s
       R False (Left e) -> R False $ Left e
       R True r -> R True $
         case r of
           Left e -> Left $ expected x e
           Right (a,s1)
             | x == a    -> inner xs s1
             | otherwise -> Left $ expected x $ unexpected a $ statePos s

  -- we consumed something
  inner [] s      = Right ((),s)
  inner (x:xs) s  =
    case unP (labels p [sh x]) s of
      R _ (Right (a,s1))
        | x == a    -> inner xs s1
        | otherwise -> Left $ expected x $ unexpected a $ statePos s
      R _ (Left e)  -> Left e



-- | We use to let the user know that we have entered an infinity loop.
crash :: String -> a
crash f = error $ f ++ " applied to a parser  that accepts the empty string."

-- Instances -------------------------------------------------------------------

instance Functor (Parser t) where
  fmap = liftM

instance Monad (Parser t) where
  return a  = pure a
  p >>= f   = P $ \s ->
    case unP p s of
      R True r  -> R True $ case r of
                              Left e -> Left e
                              Right (a,s1) ->
                                case unP (f a) s1 of
                                  R _ r1 -> r1
      R False r -> case r of
                     Left e -> R False $ Left e
                     Right (a,s1) -> unP (f a) s1

  fail m  = parseError (newErrorMessage (Message m))

instance Applicative (Parser t) where
  pure a  = P $ \s -> R False $ Right (a,s)
  (<*>)   = ap

instance Alternative (Parser t) where
  empty     = parseError newErrorUnknown
  p1 <|> p2 = P $ \s ->
    -- WARNING: It is important that we match the 'False' first
    -- because then we can quickly move to the second branch, without
    -- having to perform any actual parsing.
    case unP p1 s of
      R False (Left _) -> unP p2 s
      other            -> other


instance MonadPlus (Parser t) where
  mzero   = empty
  mplus   = (<|>)




