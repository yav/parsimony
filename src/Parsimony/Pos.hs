-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Pos
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Textual source positions.
--
-----------------------------------------------------------------------------

module Parsimony.Pos
  ( SourceName, Line, Column
  , SourcePos
  , sourceLine, sourceColumn, sourceName
  , incSourceLine, incSourceColumn
  , setSourceLine, setSourceColumn, setSourceName
  , newPos, initialPos
  , updatePosChar, updatePosString
  ) where

-----------------------------------------------------------
-- Source Positions, a file name, a line and a column.
-- upper left is (1,1)
-----------------------------------------------------------
type SourceName     = String
type Line           = Int
type Column         = Int

data SourcePos      = SourcePos { sourceName    :: SourceName
                                , sourceLine    :: !Line
                                , sourceColumn  :: !Column
                                }
                      deriving (Eq,Ord)

newPos :: SourceName -> Line -> Column -> SourcePos
newPos name line column = SourcePos { sourceName    = name
                                    , sourceLine    = line
                                    , sourceColumn  = column
                                    }

initialPos         :: SourceName -> SourcePos
initialPos name     = newPos name 1 1

incSourceLine      :: SourcePos -> Line -> SourcePos
incSourceLine p n   = setSourceLine p (n + sourceLine p)

incSourceColumn    :: SourcePos -> Column -> SourcePos
incSourceColumn p n = setSourceColumn p (n + sourceColumn p)

setSourceName      :: SourcePos -> SourceName -> SourcePos
setSourceName p n   = p { sourceName = n }

setSourceLine      :: SourcePos -> Line -> SourcePos
setSourceLine p n   = p { sourceLine = n }

setSourceColumn    :: SourcePos -> Column -> SourcePos
setSourceColumn p n = p { sourceColumn = n }

-----------------------------------------------------------
-- Update source positions on characters
-----------------------------------------------------------
updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string
    = forcePos (foldl updatePosChar pos string)

updatePosChar   :: SourcePos -> Char -> SourcePos
updatePosChar p c
    = forcePos $
      case c of
        '\n' -> setSourceColumn (incSourceLine p 1) 1
        '\t' -> incSourceColumn p (8 - ((sourceColumn p - 1) `mod` 8))
        _    -> incSourceColumn p 1


forcePos :: SourcePos -> SourcePos
forcePos pos@(SourcePos _ line column)
    = seq line (seq column (pos))

-----------------------------------------------------------
-- Show positions
-----------------------------------------------------------
instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")" 
