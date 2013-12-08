module Interp (Block(Function, name, args, sents, rets), Sentence(Return, Relation), Expr(Atom, Operation, Call, Pattern), Atom(Identifier, Literal), Literal(Nat, Real), DependencyType(Always, Partly), Dependency, leftHand, rightHand, interpret) where
import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Control.Applicative (pure, (<$>), (<*>), (<*), (*>))
import Control.Monad
import Control.Monad.Error


data Block = Function {
                        name :: String,
                        args :: [Expr],
                        sents :: [Sentence],
                        rets :: [Expr]
                      }
           deriving Show

data Sentence = Return Expr
              | Relation String Expr Expr
          deriving Eq

data Expr = Atom Atom
          | Operation String Expr Expr
          | Call String [Expr]
          | Pattern [(Expr, Sentence)]
          deriving Eq

data Atom = Identifier String | Literal Literal
          deriving Eq

data Literal = Nat Int | Real Double
          deriving Eq

data Type = IntType | RealType

data Connection a = Unidir a | Bidir a
                  deriving (Eq, Show)

data DependencyType = Always | Partly Sentence
                  deriving (Eq, Show)

type Dependency = (Expr, DependencyType)


instance Show Sentence where
    show (Return x)			= foldl1 (++) ["return ", show x]
    show (Relation r x y)	= foldl1 (++) [show x, " ", r, " ", show y]

instance Show Expr where
    show (Atom p)			= show p
    show (Operation o x y)	= foldl1 (++) ["(", show x, " ", o, " ", show y, ")"]
    show (Call f x)			= foldl1 (++) [f, "(", intercalate ", " (map show x), ")"]
    show (Pattern a)		= foldl1 (++) $ intercalate [", "] $ map (\(e, s) -> [show e, " (", show s, ")"]) a

instance Show Atom where
    show (Identifier s)	= s
    show (Literal l)	= show l

instance Show Literal where
    show (Nat n)	= show n
    show (Real r)	= show r


(?) :: (Error e) => Maybe a -> String -> Either e a
(Just x) ? _ = Right x
Nothing ? er = throwError $ strMsg er

leftHand :: Sentence -> Expr
leftHand (Relation _ l _) = l
rightHand :: Sentence -> Expr
rightHand (Relation _ _ r) = r


interpret :: Block -> Either String (Gr Sentence Dependency)
interpret f = dependencyGraph f


-- dependency analyzer --

dependencyGraph :: Block -> Either String (Gr Sentence Dependency)
dependencyGraph (Function _ a s r) = constrDepGraph a s (mkGraph nodes []) init
 where
     nodes = zip [0..] $ map Return r
     init = zip [0..] $ map (\x -> (x, Always)) r

constrDepGraph :: [Expr] -> [Sentence] -> Gr Sentence Dependency -> [(Node, Dependency)] -> Either String (Gr Sentence Dependency)
constrDepGraph _ _ gr [] = Right gr
constrDepGraph arg st gr xs
    | elem x arg = constrDepGraph arg st gr (tail xs)
    | isJust def = constrDepGraph arg st (insEdge (from, fst $ fromJust def, (x, dep)) gr) (tail xs)
    | otherwise  = do
        eq <- find (\s -> any (== Bidir x) $ getVarSent s) st ? ("`" ++ show x ++ "` not defined")
        expl <- solve x eq
        constrDepGraph arg (delete eq st)
                       (insEdge (from, curr, (x, dep))
                         $ insNode (curr, expl) gr)
                       (tail xs ++ (zip (repeat curr) $ (getDep . rightHand) expl))
 where
     (from, (x, dep)) = head xs
     curr = head $ newNodes 1 gr
     def = find (\(n, l) -> isRel l && leftHand l == x) $ labNodes gr
     isRel (Relation _ _ _) = True
     isRel _ = False
     undir (Bidir a)  = a
     undir (Unidir a) = a


getDepSent :: Sentence -> [Dependency]
getDepSent (Relation _ x y) = getDep x ++ getDep y
getDepSent (Return x)       = getDep x

getDep :: Expr -> [Dependency]
getDep (Operation _ x y) = getDep x ++ getDep y
getDep (Call _ a)        = concatMap getDep a
getDep (Pattern a)       = concatMap (getDepSent . snd) a ++ concatMap (\(x, c) -> map (cond c) (getDep x)) a
 where
     cond s (x, _) = (x, Partly s)
getDep x | isIdent x     = [(x, Always)]
         | otherwise     = []


direct :: Connection a -> Connection a
direct (Bidir x) = Unidir x
direct x = x

getVarSent :: Sentence -> [Connection Expr]
getVarSent (Relation "=" x y) = getVar x ++ getVar y
getVarSent (Relation _ x y)   = map direct $ getVar x ++ getVar y
getVarSent (Return x)         = getVar x

getVar :: Expr -> [Connection Expr]
getVar (Operation _ x y) = getVar x ++ getVar y
getVar (Call _ a)        = map direct $ concatMap getVar a
getVar (Pattern a)       = map direct $ concatMap (\(x, c) -> getVar x ++ getVarSent c) a
getVar x | isIdent x     = [Bidir x]
         | otherwise     = []


isIdent :: Expr -> Bool
isIdent (Atom (Identifier _)) = True
isIdent _ = False


-- equation solver --

solve :: Expr -> Sentence -> Either String Sentence
solve x (Relation "=" y z) =
    Relation "=" x <$> xor (solveExpr x y z) (solveExpr x z y)
solve _ _ = throwError $ noMsg

xor :: (Error e) => Either e b -> Either e b -> Either e b
xor (Right x) (Left _) = Right x
xor (Left _) (Right x) = Right x
xor _ _ = throwError $ strMsg "solving failed"

solveExpr :: Expr -> Expr -> Expr -> Either String Expr
solveExpr x y z | z == x = Right y
solveExpr x y (Atom a) = throwError noMsg
solveExpr x y (Operation o p q) =
    xor (solveExpr x (invLeft o p y) q)
        (solveExpr x (invRight o q y) p)
solveExpr x y _ = throwError noMsg

invLeft :: String -> Expr -> Expr -> Expr
invLeft "+" p y = Operation "-" y p
invLeft "-" p y = Operation "-" p y
invLeft "*" p y = Operation "/" y p
invLeft "/" p y = Operation "/" p y

invRight :: String -> Expr -> Expr -> Expr
invRight o p y = Operation i y p
 where
     i = case o of
         "+" -> "-"
         "-" -> "+"
         "*" -> "/"
         "/" -> "*"

