-----------------------------------------------------------------------------
-- |
-- Module      :  Parsimony.Error
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Parse errors
--
-----------------------------------------------------------------------------

module Parsimony.Error
  ( Message(SysUnExpect,UnExpect,Expect,Message)
  , messageString, messageCompare, messageEq

  , ParseError, errorPos, errorMessages, errorIsUnknown
  , showErrorMessages

  , newErrorMessage, newErrorUnknown
  , addErrorMessage, setErrorPos, setErrorMessage
  , mergeError
  )
  where


import Parsimony.Pos
import Data.List (nub,sortBy)

-----------------------------------------------------------
-- Messages
-----------------------------------------------------------
data Message        = SysUnExpect !String   --library generated unexpect
                    | UnExpect    !String   --unexpected something
                    | Expect      !String   --expecting something
                    | Message     !String   --raw message

messageToEnum :: Message -> Int
messageToEnum msg
    = case msg of SysUnExpect _ -> 0
                  UnExpect _    -> 1
                  Expect _      -> 2
                  Message _     -> 3

messageCompare :: Message -> Message -> Ordering
messageCompare msg1 msg2
    = compare (messageToEnum msg1) (messageToEnum msg2)

messageString :: Message -> String
messageString msg
    = case msg of SysUnExpect s -> s
                  UnExpect s    -> s
                  Expect s      -> s
                  Message s     -> s

messageEq :: Message -> Message -> Bool
messageEq msg1 msg2
    = (messageCompare msg1 msg2 == EQ)


-----------------------------------------------------------
-- Parse Errors
-----------------------------------------------------------
data ParseError     = ParseError !SourcePos [Message]

errorPos :: ParseError -> SourcePos
errorPos (ParseError pos _) = pos

errorMessages :: ParseError -> [Message]
errorMessages (ParseError _ msgs)
    = sortBy messageCompare msgs

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError _ msgs)
    = null msgs


-----------------------------------------------------------
-- Create parse errors
-----------------------------------------------------------
newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos
    = ParseError pos []

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage msg pos
    = ParseError pos [msg]

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:msgs)

setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos pos (ParseError _ msgs)
    = ParseError pos msgs

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:filter (not . messageEq msg) msgs)


mergeError :: ParseError -> ParseError -> ParseError
mergeError (ParseError pos msgs1) (ParseError _ msgs2)
    = ParseError pos (msgs1 ++ msgs2)



-----------------------------------------------------------
-- Show Parse Errors
-----------------------------------------------------------
instance Show ParseError where
  show err
    = show (errorPos err) ++ ":" ++
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)


-- | Language independent show function
showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1)   = span (messageEq (SysUnExpect "")) msgs
      (unExpect,msgs2)      = span (messageEq (UnExpect "")) msgs1
      (expect,messages)     = span (messageEq (Expect "")) msgs2

      showExpect        = showMany msgExpecting expect
      showUnExpect      = showMany msgUnExpected unExpect
      showSysUnExpect
        | not (null unExpect) || null sysUnExpect       = ""
        | null firstMsg          = msgUnExpected ++ " " ++ msgEndOfInput
        | otherwise              = msgUnExpected ++ " " ++ firstMsg
           where
           firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages


      --helpers
      showMany pre ms   = case clean (map messageString ms) of
                            [] -> ""
                            cms | null pre  -> commasOr cms
                                | otherwise -> pre ++ " " ++ commasOr cms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = seperate ", " . clean
      -- semiSep           = seperate "; " . clean

      seperate _ []       = ""
      seperate _ [m]      = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = nub . filter (not.null)

