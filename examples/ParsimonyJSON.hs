module ParsimonyJSON(run,p_value) where

import Text.JSON.Types
import Parsimony
import Parsimony.Char
import Parsimony.Error
import Control.Monad
import Data.Char
import Numeric

run              :: String -> JSValue
run txt           = case parse p_value txt of
                      Left err -> error (show err)
                      Right a  -> a

p_value          :: Parser String JSValue
p_value           = spaces *> p_jvalue

tok              :: Parser String a -> Parser String a
tok p             = p <* spaces

p_jvalue         :: Parser String JSValue
p_jvalue          =  (JSNull      <$  p_null)
                 <|> (JSBool      <$> p_boolean)
                 <|> (JSArray     <$> p_array)
                 <|> (JSString    <$> p_js_string)
                 <|> (JSObject    <$> p_js_object)
                 <|> (JSRational False <$> p_number)
                 <?> "JSON value"

p_null           :: Parser String ()
p_null            = tok (string "null") >> return ()

p_boolean        :: Parser String Bool
p_boolean         = tok
                      (  (True  <$ string "true")
                     <|> (False <$ string "false")
                      )

p_array          :: Parser String [JSValue]
p_array           = between (tok (char '[')) (tok (char ']'))
                  $ p_jvalue `sepBy` tok (char ',')

p_string         :: Parser String String
p_string          = between (tok (char '"')) (char '"') (many p_char)
  where p_char    =  (char '\\' >> p_esc)
                 <|> (satisfy (\x -> x /= '"' && x /= '\\'))

        p_esc     =  ('"'   <$ char '"')
                 <|> ('\\'  <$ char '\\')
                 <|> ('/'   <$ char '/')
                 <|> ('\b'  <$ char 'b')
                 <|> ('\f'  <$ char 'f')
                 <|> ('\n'  <$ char 'n')
                 <|> ('\r'  <$ char 'r')
                 <|> ('\t'  <$ char 't')
                 <|> (char 'u' *> p_uni)
                 <?> "escape character"

        p_uni     = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = pure (toEnum code)
                        | otherwise         = empty
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)

p_object         :: Parser String [(String,JSValue)]
p_object          = between (tok (char '{')) (tok (char '}'))
                  $ p_field `sepBy` tok (char ',')
  where p_field   = (,) <$> (p_string <* tok (char ':')) <*> p_jvalue

p_number         :: Parser String Rational
p_number          = do s <- getInput
                       case readSigned readFloat s of
                         [(n,s1)] -> n <$ setInput s1
                         _        -> empty

p_js_string      :: Parser String JSString
p_js_string       = toJSString <$> p_string

p_js_object      :: Parser String (JSObject JSValue)
p_js_object       = toJSObject <$> p_object

