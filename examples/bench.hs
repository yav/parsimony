import qualified ParsimonyJSON as Parsimony
import qualified ParsecJSON as Parsec
import Parsimony.IO
import System.Environment

main =
  do as <- getArgs
     case as of
       a : f : _
          | a == "parsec" ->
              do txt <- readFile f
                 print (Parsec.run txt)
          | a == "parsimony" ->
              do txt <- readFile f
                 print (Parsimony.run txt)
       _ -> error "usage: (parsec|parsimony) file"



