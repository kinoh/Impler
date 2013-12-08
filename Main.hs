module Main where
import Control.Monad
import Data.Either
import Parser (parseInput, expr)
import Interp (Sentence, Expr, Atom, interpret)
import Trans (translate) --(reduceGraph)

main =
    fmap (\x -> leftToStr (parseInput x) >>= interpret >>= return . translate) getLines
    >>= outputStr

outputStr :: Either String String -> IO ()
outputStr (Left err) = putStrLn err
outputStr (Right a) = putStrLn a

output :: Show a => Either String a -> IO ()
output (Left err) = putStrLn err
output (Right a) = print a

leftToStr :: Show e => Either e a -> Either String a
leftToStr (Left err) = Left $ show err
leftToStr (Right x) = Right x

getLines :: IO String
getLines = getLine >>= next
 where
     next "" = return ""
     next s  = liftM ((s ++ "\n") ++) getLines
