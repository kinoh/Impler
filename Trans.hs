module Trans where
import Control.Applicative (pure, (<$>), (<*>), (<*), (*>))
import Data.Maybe
import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Interp (Block(Function), Sentence(Return, Relation), Expr(Atom, Operation, Call, Pattern), Atom(Identifier, Literal), Literal(Nat, Real), DependencyType(Always, Partly), Dependency, leftHand, rightHand)


data Flux = Cascade [Sentence] | Branch Flux [(Sentence, [Flux])]
          deriving Show

--type Language = (String, [Library])
--type Library = (String, [FuncInfo])
--type FuncInfo = (String, [Type], Type)


label gr = fromJust . lab gr

labInits :: Gr a b -> [LNode a]
labInits gr = filter (null . pre gr . fst) (labNodes gr)

ginits :: Gr a b -> [Node]
ginits gr = map fst $ labInits gr

replaceNodes :: DynGraph gr => gr a b -> [Node] -> a -> gr a b
replaceNodes g ns l =
    delNodes ns
    $ insEdges (map (\ (x, m, e) -> (n, m, e)) $ concatMap (out g) ns)
    $ insEdges (map (\ (m, x, e) -> (m, n, e)) $ concatMap (inn g) ns)
    $ insNode (n, l) g
 where
     n = head $ newNodes 1 g


-- translator --

--translate :: Language -> [Sentence] -> String
translate :: Gr Sentence Dependency -> String
translate = concatMap trans . serializeGraph . reduceGraph

trans :: Flux -> String
trans (Cascade s)  = concatMap (\t -> transSent t ++ ";\n") $ reverse s
--trans (Branch x a) = concatMap (\ (s, p) -> ) a

transSent (Return x)       = "return " ++ transExpr x
transSent (Relation r x y) = intercalate " " [transExpr x, r, transExpr y]

transExpr (Atom a)          = transAtom a
transExpr (Operation o x y) = intercalate " " [transExpr x, o, transExpr y]
transExpr (Call f a)        = f ++ (intercalate ", " $ map transExpr a)
--transExpr (Pattern p)       = 

transAtom (Identifier s) = s
transAtom (Literal l)    = transLiteral l

transLiteral (Nat n)  = show n
transLiteral (Real r) = show r


-- graph serializer --

serializeGraph :: Gr a b -> [a]
serializeGraph gr = map (label gr) $ reverse $ foldr (serialize gr) [] (ginits gr)

serialize :: Gr a b -> Node -> [Node] -> [Node]
serialize gr cur def
    | defined cur = def
    | otherwise   = cur : foldr (serialize gr) def (suc gr cur)
 where
     defined x = elem x def


-- restructure graph --

reduceGraph :: Gr Sentence Dependency -> Gr Flux Dependency
reduceGraph gr = branch . nmap Cascade . cascade $ nmap return gr
 where
    branch g = foldl reduceBranch g (labNodes g)
    cascade g = foldl reduceCascade g (labEdges g)

reduceCascade :: Gr [a] b -> LEdge b -> Gr [a] b
reduceCascade gr (n1, n2, e12)
    | isJust lx && mono (out gr n1) && mono (inn gr n2)
                = replaceNodes gr [n1, n2] (fromJust lx)
    | otherwise = gr
 where
     mono x = length x == 1
     lx = do
         l1 <- lab gr n1
         l2 <- lab gr n2
         Just (l1 ++ l2)

reduceBranch :: Gr Flux Dependency -> LNode Flux -> Gr Flux Dependency
reduceBranch gr (n1, l1)
    | gelem n1 gr && not (null parts)
                = replaceNodes gr (n1 : concatMap fst parts) (Branch l1 $ map (\ (ns, s) -> (s, map (label gr) ns)) parts)
    | otherwise = gr
 where
     s = lsuc gr n1
     ds = map (\a -> (map fst a, snd $ snd $ head a)) $ groupBy (\ (n, d1) (m, d2) -> d1 == d2) s
            :: [([Node], DependencyType)]
     parts = mapMaybe dep ds
     dep (_, Always)    = Nothing
     dep (ns, Partly s) = Just (ns, s)
