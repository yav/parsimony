-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- The basics of the Parsimony library.
--
-----------------------------------------------------------------------------

module Parsimony
  ( -- * Basic Types
    Parser

    -- * Applying Parsers
  , parse, parseSource, runParser

    -- * Choices
  , (<|>), try, choice

    -- * Repetition
  , many, many1, skipMany1, match
  , sepBy, sepBy1
  , endBy, endBy1
  , sepEndBy, sepEndBy1
  , manyTill
  , count
  , foldMany
  
    -- * Optoinal content
  , option, optional

    -- * Delimeters and Combinators
  , (<*>), (<*), (*>), (<$>), (<$), pure
  , between, skip, eof

    -- * Look Ahead
  , notFollowedBy, notFollowedBy'
  , lookAhead, anyToken

    -- * Errors 
  , (<?>), unexpected, empty, parseError, labels 

    -- * Parser State
  , State(..)
  , setState, updateState, mapState
  , getInput, setInput, updateInput
  , getPosition, setPosition, updatePosition


    -- * Primitive Parsers
  , PrimParser
  , primParser
  ) where

import Control.Applicative hiding(many)
import Parsimony.Prim
import Parsimony.Combinator

