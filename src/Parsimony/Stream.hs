{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}     -- Why?

-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Char
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- A generic way to extract tokens from a stream.
--
-----------------------------------------------------------------------------



module Parsimony.Stream
  ( Token(..), Stream(..)
  , ASCII, UTF8, ascii, utf8
  ) where

import Parsimony.Prim
import Parsimony.Pos
import Parsimony.Error

import qualified Data.ByteString as Strict (ByteString,uncons)
import qualified Data.ByteString.Lazy as Lazy (ByteString,uncons)
import Data.String.UTF8 (UTF8,UTF8Bytes,fromRep,replacement_char)
import qualified Data.String.UTF8 as UTF8 (uncons)
import Data.Word (Word8)
import Numeric (showHex)

-- | A class describing useful token operations.
class Token token where
  -- | How tokens affect file positions.
  updatePos :: token -> SourcePos -> SourcePos

  -- | How to display tokens.
  showToken :: token -> String

instance Token Char where
  updatePos c p = updatePosChar p c
  showToken     = show

instance Token Word8 where
  updatePos _ p = incSourceColumn p 1
  showToken b   = "0x" ++ showHex b ""




-- We have the fun. dep. here because otherwise multiple
-- reads from a stream could give potentially different types of
-- tokens which leads to ambiguities.

-- | Streams of tokens.
class Token token => Stream stream token | stream -> token where
  getToken :: PrimParser stream token

eof_err :: SourcePos -> Reply s a
eof_err p = Error $ newErrorMessage (UnExpect "end of input") p

{-# INLINE genToken #-}
genToken :: Token t => (i -> Maybe (t,i)) -> PrimParser i t
genToken unc (State i p) =
    case unc i of
      Nothing     -> eof_err p
      Just (t,ts) -> Ok t State { stateInput = ts
                                , statePos   = updatePos t p
                                }

instance Token a => Stream [a] a where
  getToken = genToken (\xs -> case xs of
                                [] -> Nothing
                                c : cs -> Just (c,cs))

instance Stream Strict.ByteString Word8 where
  getToken = genToken Strict.uncons

instance Stream Lazy.ByteString Word8 where
  getToken = genToken Lazy.uncons

-- Character encodings ---------------------------------------------------------


-- | The type of ASCII encoded content.
newtype ASCII content = ASCII content

-- | Specify ASCII encoding for some content.
ascii :: content -> ASCII content
ascii = ASCII

-- | Specify UTF8 encoding for some content.
utf8 :: content -> UTF8 content
utf8 = fromRep

instance Stream a Word8 => Stream (ASCII a) Char where
  getToken (State (ASCII buf) p) =
    case getToken (State buf p) of
      Error err           -> Error err
      Ok w (State b1 p1)  -> Ok (toEnum (fromEnum w)) (State (ASCII b1) p1)


instance Stream (UTF8 [Word8]) Char where
  getToken = genTokenChar

instance Stream (UTF8 Strict.ByteString) Char where
  getToken = genTokenChar

instance Stream (UTF8 Lazy.ByteString) Char where
  getToken = genTokenChar


{-# INLINE genTokenChar #-}
genTokenChar :: UTF8Bytes stream ix => PrimParser (UTF8 stream) Char
genTokenChar (State i p) =
    case UTF8.uncons i of
      Just (a,i1)
        | a /= replacement_char -> Ok a (State i1 (updatePos a p))
        | otherwise -> Error $ newErrorMessage
                              (Message "invalid UTF8 character") p
      Nothing -> eof_err p


