{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}   -- Why ???

-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.UserState
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- Support for parsers with custom state.
--
-----------------------------------------------------------------------------




module Parsimony.UserState
  ( ParserU, UserState(..)
  , lifted
  , getUserState, setUserState, updateUserState
  , uparse, uparseSource
  ) where

import Parsimony.Prim
import Parsimony.Stream
import Parsimony.Pos
import Parsimony.Error
import Parsimony.Combinator

-- NOTE: We could generalize this further by providing
-- a class that abstract over 'extract' and 'inject'

-- | An input stream annotated with some user state.
data UserState user stream  = UserState { userState    :: !user
                                        , parserStream :: !stream
                                        }

-- | The type of parsers with a user state.
type ParserU u s = Parser (UserState u s)


extract :: State (UserState u s) -> (State s, u)
extract s = (s { stateInput = xs }, u)
  where UserState u xs = stateInput s

inject :: State s -> u -> State (UserState u s)
inject s u = s { stateInput = UserState u (stateInput s) }

instance Stream stream token => Stream (UserState user stream) token where
  getToken s =
    case extract s of
      (s1,u) ->
        case getToken s1 of
          Left err      -> Left err
          Right (a,s2)  -> Right (a, inject s2 u)

-- | Turn a parser without user space into ine that supports
-- user state manipulation.
lifted             :: Parser s a -> ParserU u s a
lifted              = mapState extract inject

-- | Get the user state.
getUserState       :: ParserU u s u
getUserState        = userState `fmap` getInput

-- | Set the user state.
setUserState       :: u -> ParserU u s ()
setUserState u      = updateInput (\i -> i { userState = u })

-- | Update the user state.
updateUserState    :: (u -> u) -> ParserU u s ()
updateUserState f   = updateInput (\i -> i { userState = f (userState i) })

uparse             :: ParserU u s a -> u -> s -> Either ParseError a
uparse p u          = uparseSource p u ""

uparseSource       :: ParserU u s a -> u -> SourceName -> s
                   -> Either ParseError a
uparseSource p u n s  = parseSource p n (UserState u s)

