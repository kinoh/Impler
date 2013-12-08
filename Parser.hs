module Parser (parseInput, expr) where
import Control.Applicative hiding ((<|>), many, optional)
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Interp


parseInput :: String -> Either ParseError Block
parseInput = parse block "Input Parser"

parens = between (char '(') (char ')')


trySepBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
trySepBy p sep =
    many (try (p <* sep <* lookAhead p) <|> p)

trySepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
trySepBy1 p sep =
    (:) <$> p <* sep <*> trySepBy p sep


block :: Parser Block
block =
    Function
    <$> ident
    <* spaces
    <*> parens (sepBy expr (char ',' *> spaces))
    <* many1 newline
    <*> (concat <$> trySepBy defs (many1 newline))
    <* many1 newline
    <*> (string "return" *> spaces *> sepBy expr (char ',' *> spaces))

{-
sentence :: Parser [Sentence]
sentence =
    listen <$> (declare <|> ret)
    <|> definitions
 where
     listen x = [x]

declare :: Parser Sentence
declare = try (Declare <$> ident <* spaces <*> parens (sepBy expr (char ',' *> spaces)))

ret :: Parser Sentence
ret = try (Return <$> (string "return" *> spaces *> expr))
-}
defs :: Parser [Sentence]
defs = many1 (try (expr <**> (Relation <$> relOp) <*> lookAhead (expr <* relOp))
               <|> (expr <**> (Relation <$> relOp) <*> expr))

relOp :: Parser String
relOp = choice $ map (\s -> try $ string s) [ "=", "/=", ">=", "<=", ">", "<" ]

expr :: Parser Expr
expr = try exprPattern <|> try exprCall <|> exprAdd

exprPattern :: Parser Expr
exprPattern =
    Pattern <$> trySepBy1 ((\e s -> (e, s)) <$> ex <* spaces <*> parens st) ((char ',' <|> newline) <* spaces)
 where
     st = ex <**> (Relation <$> relOp) <*> ex
     ex = try exprCall <|> exprAdd

exprCall :: Parser Expr
exprCall = Call <$> ident <*> parens (sepBy exprAdd (char ','))

exprAdd :: Parser Expr
exprAdd =
    chainl1 exprMul (Operation <$> string "+"
                      <|> Operation <$> string "-")

exprMul :: Parser Expr
exprMul =
    chainl1 (Atom <$> atom
              <|> pos . Atom <$> (string "+" *> atom)
              <|> neg . Atom <$> (string "-" *> atom)
              <|> parens exprAdd)
            (try (Operation <$> string "/" <* notFollowedBy (char '='))
              <|> Operation <$> string "*")
 where
     pos = Operation "+" ((Atom . Literal . Nat) 0)
     neg = Operation "-" ((Atom . Literal . Nat) 0)

atom :: Parser Atom
atom =
    (Literal <$> (try real <|> nat))
    <|> (Identifier <$> ident)

ident :: Parser String
ident = many1 (letter <|> digit)

nat :: Parser Literal
nat = Nat . parseInt <$> many1 digit

real :: Parser Literal
real = (\x y -> Real $ parseReal x y) <$> many1 digit <* char '.' <*> many digit


parseInt :: [Char] -> Int
parseInt = (foldl1 ((+) . (* 10))) . (map digitToInt)

parseReal :: [Char] -> [Char] -> Double
parseReal x y = (fromIntegral $ parseInt x) + (fromIntegral $ parseInt y) / 10.0 ** (fromIntegral $ length y)
