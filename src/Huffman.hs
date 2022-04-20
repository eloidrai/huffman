module Huffman (createHuffman, getCodes, display, encode) where

import Data.List (sort)

data Tree = Leaf {weight :: Integer, value :: Char} | Node {weight :: Integer, children :: (Tree, Tree)}

instance Show Tree where
  show t = init $ s t 0
    where
      s (Leaf w v) depth = (take depth (repeat ' ')) ++ " [" ++ (show w) ++ "]:" ++ (show v) ++ "\n"
      s (Node w (c1, c2)) depth = (take depth (repeat ' ')) ++ "*[" ++ (show w) ++ "]\n" ++ concat [s child (depth+1) | child <- [c1, c2]]

instance Eq Tree where
  a == b = weight a == weight b

instance Ord Tree where
  compare a b = if (weight a /= weight b) then compare (weight a) (weight b) else compare (value (lastChild a)) (value (lastChild b))
    where
      lastChild :: Tree -> Tree
      lastChild (Leaf weight value) = Leaf weight value
      lastChild (Node weight children) = lastChild $ fst children

merge :: Tree -> Tree -> Tree
merge tree1 tree2 = Node (weight tree1 + weight tree2) (tree1, tree2)

counter :: Eq a => [a] -> [(a, Integer)]
counter l = foldl addChar [] l
  where
    addChar :: Eq a => [(a, Integer)] -> a -> [(a, Integer)]
    addChar dict val = (val, maybe 0 id (lookup val dict) + 1):(filter (\(k, v) -> k/=val) dict)

createHuffman :: String -> Tree
createHuffman string = (head.process.sort.start) string
  where
    start :: String -> [Tree]
    start string = map (\(char, count) -> Leaf count char) (counter string)
    process :: [Tree] -> [Tree]
    process (t1:[]) = [t1]
    process (t1:t2:nodes) = process $ sort $ (merge t1 t2):nodes

getCodes :: Tree -> String -> [(Char, String)]
getCodes (Leaf w v) code = [(v, code)]
getCodes (Node w (c1, c2)) code = concat [getCodes c1 (code++"0"), getCodes c2 (code++"1")]

encode :: Tree -> String -> String
encode tree string  = concat $ map (\c -> maybe "" id (lookup c dict)) string
  where
    dict = getCodes tree ""

display tree = do
  putStrLn "digraph g {labelloc=\"t\"\n"
  (putStr.concat) (map displayName names)
  (putStr.concat) (map displayEdges edges)
  putStrLn "}"
  where
    names = listNames tree 1
    displayName (n, label, kind) = (show n) ++ "[label=" ++ (show label) ++ (if (kind == 1) then ", shape=record" else "") ++ "]\n"
    edges = listEdges tree 1
    displayEdges (r, (c1, c2)) = (show r) ++ " -> {" ++ (show c1) ++ "," ++ (show c2)++ "};\n"
    listNames (Node w (c1, c2)) n = (n, show w, 0):((listNames c1 (2*n)) ++ (listNames c2 (2*n+1)))
    listNames (Leaf w char) n = [(n, ('\'':char:"' - " ++ (show w)), 1)]
    listEdges (Node w (c1, c2)) n = (n, (2*n, 2*n+1)):((listEdges c1 (2*n)) ++ (listEdges c2 (2*n+1)))
    listEdges (Leaf w char) n = []
