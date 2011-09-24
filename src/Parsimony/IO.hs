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
  ( parseFile
  , parseLargeFile
  , parseBinaryFile
  , parseLargeBinaryFile

  , uparseFile
  , uparseLargeFile
  , uparseBinaryFile
  , uparseLargeBinaryFile
  ) where

import Parsimony.Prim
import Parsimony.Error
import Parsimony.Combinator
import Parsimony.UserState

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

-- | Parse a text file in one go.
-- This functions loads the whole file in memory.
parseFile :: FilePath -> Parser T.Text a -> IO (Either ParseError a)
parseFile f p = parseSource p f `fmap` T.readFile f

-- | Parse a text file in chunks.
-- This functions loads the file in chunks.
parseLargeFile :: FilePath -> Parser LT.Text a -> IO (Either ParseError a)
parseLargeFile f p = parseSource p f `fmap` LT.readFile f

-- | Parse a binary file in one go.
-- This functions loads the whole file in memory.
parseBinaryFile :: FilePath -> Parser Strict.ByteString a ->
                                                IO (Either ParseError a)
parseBinaryFile f p = parseSource p f `fmap` Strict.readFile f

-- | Parse a text file in chunks.
-- This functions loads the file in chunks.
parseLargeBinaryFile :: FilePath -> Parser Lazy.ByteString a ->
                                                    IO (Either ParseError a)
parseLargeBinaryFile f p = parseSource p f `fmap` Lazy.readFile f


-- With user state -------------------------------------------------------------



-- | Parse a text file in one go, using user state.
-- This functions loads the whole file in memory.
uparseFile :: FilePath -> ParserU u T.Text a -> u -> IO (Either ParseError a)
uparseFile f p u = uparseSource p u f `fmap` T.readFile f

-- | Parse a text file in chunks, using user state.
-- This functions loads the file in chunks.
uparseLargeFile :: FilePath -> ParserU u LT.Text a ->
                                            u -> IO (Either ParseError a)
uparseLargeFile f p u = uparseSource p u f `fmap` LT.readFile f

-- | Parse a binary file in one go, using user state.
-- This functions loads the whole file in memory.
uparseBinaryFile :: FilePath -> ParserU u Strict.ByteString a ->
                                             u -> IO (Either ParseError a)
uparseBinaryFile f p u = uparseSource p u f `fmap` Strict.readFile f

-- | Parse a text file in chunks, using user state.
-- This functions loads the file in chunks.
uparseLargeBinaryFile :: FilePath -> ParserU u Lazy.ByteString a ->
                                             u -> IO (Either ParseError a)
uparseLargeBinaryFile f p u = uparseSource p u f `fmap` Lazy.readFile f




