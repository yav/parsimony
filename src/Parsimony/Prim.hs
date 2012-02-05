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
  ( Parser, PrimParser, Reply(..)
  , runParser, primParser
  , parseError, try, lookAhead, labels
  , foldMany, foldManyWhile, skipMany, match
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
data R s a            = R !Bool (Reply s a)

data Reply s a        = Ok !a !(State s)
                      | Error !ParseError


-- | The parser state.
data State t          = State { stateInput :: !t          -- ^ Token source
                              , statePos   :: !SourcePos  -- ^ Current position
                              }

type PrimParser s a   = State s -> Reply s a

-- | Define a primitive parser.
-- Consumes input on success.
{-# INLINE primParser #-}
primParser           :: PrimParser t a -> Parser t a
primParser prim       = P $ \s -> case prim s of
                                    r@(Error _) -> R False r
                                    r           -> R True r


{-# INLINE runParser #-}
-- | Convert a parser into a 'PrimParser'.
runParser            :: Parser t a -> PrimParser t a
runParser p s         = case unP p s of
                          R _ x -> x

-- | Access the current parser state.
-- Does not consume input.
{-# INLINE getState #-}
getState             :: Parser t (State t)
getState              = P $ \s -> R False (Ok s s)

-- | Modify the current parser state.
-- Returns the old state.
-- Does not consume input.
{-# INLINE updateState #-}
updateState          :: (State s -> State s) -> Parser s ()
updateState f         = P $ \s -> R False $! Ok () (f s)

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
        -- XXX: strict
        R c r -> R c $ case r of
                         Error err    -> Error err
                         Ok a small1  -> Ok a (inject small1 extra)



-- | Fail with the given parser error without consuming any input.
-- The error is applied to the current source position.
{-# INLINE parseError #-}
parseError          :: (SourcePos -> ParseError) -> Parser t a
parseError e         = P $ \s -> R False $ Error $ e $ statePos s




-- | Allow a parser to back-track.  The resulting parser behaves like
-- the input parser unless it fails.  In that case,  we backtrack
-- without consuming any input.  Because we may have to back-track,
-- we keep a hold of the parser input so over-use of this function
-- may result in memory leaks.

{-# INLINE try #-}
try                :: Parser t a -> Parser t a
try p               = P $ \s ->
  case unP p s of
    R True (Error err)  -> R False $ Error $ setErrorPos (statePos s) err
    other               -> other


-- | Applies the given parser without consuming any input.
{-# INLINE lookAhead #-}
lookAhead          :: Parser t a -> Parser t a
lookAhead p         = P $ \s ->
  R False $ case unP p s of
              R _ (Error err) -> Error err
              R _ (Ok a _)    -> Ok a s

-- | The resulting parser behaves like the input parser,
-- except that in case of failure we use the given expectation
-- messages.
{-# INLINE labels #-}
labels             :: Parser t a -> [String] -> Parser t a
labels p msgs0      = P $ \s ->
  case unP p s of
    R False r -> R False (addErr r)
    other     -> other

  where setExpectErrors err []         = setErrorMessage (Expect "") err
        setExpectErrors err [msg]      = setErrorMessage (Expect msg) err
        setExpectErrors err (msg:msgs) =
          foldr (\m e -> addErrorMessage (Expect m) e)
             (setErrorMessage (Expect msg) err) msgs

        addErr (Error e)  = Error $ setExpectErrors e msgs0
        addErr r          = r


-- | Apply a parser repeatedly, combining the results with the
-- given functions.  This function is similar to the strict 'foldl'.
-- We stop when an application of the parser fails without consuming any
-- input.  If the parser fails after it has consumed some input, then
-- the repeated parser will also fail.

{-# INLINE foldMany #-}
foldMany :: (b -> a -> b) -> b -> Parser t a -> Parser t b
foldMany cons nil p = P $ \s ->
  case unP p s of
    R False (Ok {})     -> crash "Parsimony.foldMany"
    R False (Error _)   -> R False $ Ok nil s
    R True  (Ok x s1)   -> R True  $ (walk $! cons nil x) s1
    R True  (Error err) -> R True  $ Error err

  -- NOTE: this is written like this because after the first iteration
  -- we already know weather the parser will be consuming input.
  where
  walk xs s =
    case unP p s of
      R False (Ok {})   -> crash "Parsimony.foldMany"
      R False (Error _) -> Ok xs s
      R True  (Ok x s1) -> (walk $! cons xs x) s1
      R True  (Error e) -> Error e


-- | Apply a parser repeatedly, combining the results with the
-- given functions.  This function is similar to the strict 'foldl'.
-- We stop on one of the following conditions:
--   * an application of the parser fails without consuming any input,
--   * the pearser returns 'Nothing' as a result.
-- If the parser fails after it has consumed some input, then
-- the repeated parser will also fail.

{-# INLINE foldManyWhile #-}
foldManyWhile :: (b -> a -> b) -> b -> Parser t (Maybe a) -> Parser t b
foldManyWhile cons nil p = P $ \s ->
  case unP p s of
    R False (Ok Nothing _)   -> R False $ Ok nil s
    R False (Ok {})          -> crash "Parsimony.foldManyWhile"
    R False (Error _)        -> R False $ Ok nil s
    R True  (Ok Nothing s1)  -> R True  $ Ok nil s1
    R True  (Ok (Just x) s1) -> R True  $ (walk $! cons nil x) s1
    R True  (Error err)      -> R True  $ Error err

  -- NOTE: this is written like this because after the first iteration
  -- we already know weather the parser will be consuming input.
  where
  walk xs s =
    case unP p s of
      R False (Ok Nothing _)    -> Ok xs s
      R False (Ok {})           -> crash "Parsimony.foldManyWhile"
      R False (Error _)         -> Ok xs s
      R True  (Ok Nothing s1)   -> Ok xs s1
      R True  (Ok (Just x) s1)  -> (walk $! cons xs x) s1
      R True  (Error e)         -> Error e


-- | Apply a parser repeatedly, ignoring the results.
-- We stop when an application of the parser fails without consuming any
-- input.  If the parser fails after it has consumed some input, then
-- the repeated parser will also fail.

{-# INLINE skipMany #-}
skipMany :: Parser t a -> Parser t ()
skipMany p = P $ \s ->
  -- pFold specialized for a common case

  case unP p s of
    R False (Ok {})     -> crash "Parsimony.skipMany"
    R False (Error _)   -> R False $ Ok () s
    R True  (Ok _ s1)   -> R True  $ walk s1
    R True  (Error err) -> R True  $ Error err

  -- NOTE: this is written like this because after the first iteration
  -- we already know weather the parser will be consuming input.
  where
  walk s =
    case unP p s of
      R False (Ok {})   -> crash "Parsimony.skipMany"
      R False (Error _) -> Ok () s
      R True  (Ok _ s1) -> walk s1
      R True  (Error e) -> Error e


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
  outer [] s      = R False $ Ok () s
  outer (x:xs) s  =
     case unP (labels p [sh x]) s of
       R False (Ok a s1)
         | x == a    -> outer xs s1
         | otherwise -> R False $ Error $ expected x $ unexpected a $ statePos s
       R False (Error e) -> R False $ Error e
       R True r -> R True $
         case r of
           Error e -> Error $ expected x e
           Ok a s1
             | x == a    -> inner xs s1
             | otherwise -> Error $ expected x $ unexpected a $ statePos s

  -- we consumed something
  inner [] s      = Ok () s
  inner (x:xs) s  =
    case unP (labels p [sh x]) s of
      R _ (Ok a s1)
        | x == a    -> inner xs s1
        | otherwise -> Error $ expected x $ unexpected a $ statePos s
      R _ (Error e) -> Error e



-- | We use to let the user know that we have entered an infinity loop.
crash :: String -> a
crash f = error $ f ++ " applied to a parser that accepts the empty string."

-- Instances -----------------------------------------------------------------

instance Functor (Parser t) where
  fmap = liftM

instance Monad (Parser t) where
  return a  = pure a
  p >>= f   = P $ \s ->
    case unP p s of
      R True r  -> R True $ case r of
                              Error e -> Error e
                              Ok a s1 ->
                                case unP (f a) s1 of
                                  R _ r1 -> r1
      R False r -> case r of
                     Error e  -> R False $ Error e
                     Ok a s1  -> unP (f a) s1

  fail m  = parseError (newErrorMessage (Message m))

instance Applicative (Parser t) where
  pure a  = P $ \s -> R False $ Ok a s
  (<*>)   = ap

instance Alternative (Parser t) where
  empty     = parseError newErrorUnknown
  p1 <|> p2 = P $ \s ->
    -- WARNING: It is important that we match the 'False' first
    -- because then we can quickly move to the second branch, without
    -- having to perform any actual parsing.
    case unP p1 s of
      R False (Error e) ->
        case unP p2 s of
          R c r -> R c $ case r of
                           Error e2 -> Error (mergeError e e2)
                           _        -> r
      other             -> other

instance MonadPlus (Parser t) where
  mzero   = empty
  mplus   = (<|>)




