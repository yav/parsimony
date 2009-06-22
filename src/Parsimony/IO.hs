-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.IO
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
--
-- Utilities for parsing content from files.
--
-----------------------------------------------------------------------------

module Parsimony.IO
  ( parseFileASCII
  , parseFileUTF8
  , parseLargeFileASCII
  , parseLargeFileUTF8

  , uparseFileASCII
  , uparseFileUTF8
  , uparseLargeFileASCII
  , uparseLargeFileUTF8
  ) where

import Parsimony.Prim
import Parsimony.Error
import Parsimony.Stream
import Parsimony.Combinator
import Parsimony.UserState

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

-- | Parse a file containing ASCII encoded characters.
-- This functions loads the whole file in memory.
parseFileASCII :: FilePath
               -> Parser (ASCII Strict.ByteString) a
               -> IO (Either ParseError a)
parseFileASCII f p =
  do bytes <- Strict.readFile f
     return $ parseSource p f $ ascii bytes

-- | Parse a file containing UTF8 encoded characters.
-- This functions loads the whole file in memory.
parseFileUTF8 :: FilePath
              -> Parser (UTF8 Strict.ByteString) a
              -> IO (Either ParseError a)
parseFileUTF8 f p =
  do bytes <- Strict.readFile f
     return $ parseSource p f $ utf8 bytes
      
-- | Parse a file containing ASCII encoded characters.
-- This functions loads the file in chunks.
parseLargeFileASCII :: FilePath
                    -> Parser (ASCII Lazy.ByteString) a
                    -> IO (Either ParseError a)
parseLargeFileASCII f p =
  do bytes <- Lazy.readFile f
     return $ parseSource p f $ ascii bytes

-- | Parse a file containing UTF8 encoded characters.
-- This functions loads the file in chunks.
parseLargeFileUTF8 :: FilePath
                   -> Parser (UTF8 Lazy.ByteString) a
                   -> IO (Either ParseError a)
parseLargeFileUTF8 f p =
  do bytes <- Lazy.readFile f
     return $ parseSource p f $ utf8 bytes

-- With user state -------------------------------------------------------------

-- | Parse a file containing ASCII encoded characters,
-- using a parser with custom user state.
-- This functions loads the whole file in memory.
uparseFileASCII :: FilePath
               -> ParserU u (ASCII Strict.ByteString) a
               -> u -> IO (Either ParseError a)
uparseFileASCII f p u =
  do bytes <- Strict.readFile f
     return $ uparseSource p u f $ ascii bytes

-- | Parse a file containing UTF8 encoded characters,
-- using a parser with custom user state.
-- This functions loads the whole file in memory.
uparseFileUTF8 :: FilePath
              -> ParserU u (UTF8 Strict.ByteString) a
              -> u -> IO (Either ParseError a)
uparseFileUTF8 f p u =
  do bytes <- Strict.readFile f
     return $ uparseSource p u f $ utf8 bytes
      
-- | Parse a file containing ASCII encoded characters,
-- using a parser with custom user state.
-- This functions loads the file in chunks.
uparseLargeFileASCII :: FilePath
                    -> ParserU u (ASCII Lazy.ByteString) a
                    -> u -> IO (Either ParseError a)
uparseLargeFileASCII f p u =
  do bytes <- Lazy.readFile f
     return $ uparseSource p u f $ ascii bytes

-- | Parse a file containing UTF8 encoded characters,
-- using a parser with custom user state.
-- This functions loads the file in chunks.
uparseLargeFileUTF8 :: FilePath
                   -> ParserU u (UTF8 Lazy.ByteString) a
                   -> u -> IO (Either ParseError a)
uparseLargeFileUTF8 f p u =
  do bytes <- Lazy.readFile f
     return $ uparseSource p u f $ utf8 bytes
 
 
