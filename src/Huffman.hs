module Huffman (createHuffman, getCodes, display, encode, decode) where

import Data.List (sort, sortBy, sortOn, insert)
import Data.Maybe (fromMaybe)
import Text.Printf
import Data.Bits (Bits(shift))

data Tree = Leaf {weight :: Integer, value :: Char} | Node {weight :: Integer, children :: (Tree, Tree)}

instance Show Tree where
  show t = init $ s t 0
    where
      s (Leaf w v) depth = replicate depth ' ' ++ " [" ++ show w ++ "]:" ++ show v ++ "\n"
      s (Node w (c1, c2)) depth = replicate depth ' ' ++ "*[" ++ show w ++ "]\n" ++ concat [s child (depth+1) | child <- [c1, c2]]

instance Eq Tree where
  a == b = weight a == weight b

instance Ord Tree where
  compare a b = if weight a /= weight b then compare (weight a) (weight b) else compare (value (lastChild a)) (value (lastChild b))
    where
      lastChild :: Tree -> Tree
      lastChild (Leaf weight value) = Leaf weight value
      lastChild (Node weight children) = lastChild $ fst children

merge :: Tree -> Tree -> Tree
merge tree1 tree2 = Node (weight tree1 + weight tree2) (tree1, tree2)

counter :: Eq a => [a] -> [(a, Integer)]
counter = foldl addChar []
  where
    addChar :: Eq a => [(a, Integer)] -> a -> [(a, Integer)]
    addChar dict val = (val, fromMaybe 0 (lookup val dict) + 1):filter (\(k, v) -> k/=val) dict

createHuffman :: String -> Tree
createHuffman string = (head.process.sort.start) string
  where
    start :: String -> [Tree]
    start string = map (\(char, count) -> Leaf count char) (counter string)
    process :: [Tree] -> [Tree]
    process [] = []
    process [t1] = [t1]
    process (t1:t2:nodes) = process $ sort $ merge t1 t2:nodes

getCodes :: Tree  -> [(Char, String)]
getCodes tree  = sortBy (\(a, ca) (b, cb) -> if length ca /= length cb then compare (length ca) (length cb) else compare a b) (codes tree "")
  where
    codes (Leaf w v) code = [(v, code)]
    codes (Node w (c1, c2)) code = codes c1 (code++"0") ++ codes c2 (code++"1")

-- Returns integers representing the codes
getCannonicalCodes :: Tree -> [(Char, Int)]
getCannonicalCodes tree = reverse l'
  where
    l = sortOn snd $ sortOn fst $  map (\(c, code) -> (c, length code)) (getCodes tree)
    l' :: [(Char, Int)]
    l' = foldl f [] l
    f :: [(Char, Int)] -> (Char, Int) ->  [(Char, Int)]
    f [] (char, size) = [(char, 0)]
    f acc@((_, lc):t) (char, size) = (char, c) : acc
      where
        pnc = lc+1
        s = size - (floor.(+1).logBase 2) (fromIntegral pnc) :: Int
        c = shift pnc s

encode :: Tree -> String -> String
encode tree = concatMap (\c -> fromMaybe "" (lookup c dict))
  where
    dict = getCodes tree

decode :: Tree -> String -> String
decode tree string = fst $ foldl dec ([], "") string
  where
    dict = map (\(char, code) -> (code, char)) (getCodes tree)
    dec (ok, partial) bit = maybe (ok, partial++[bit]) (\char ->  (ok++[char], "")) (lookup (partial++[bit]) dict)


display :: Tree -> IO ()
display tree = do
  putStrLn "digraph g {labelloc=\"t\"\n"
  putStr (concatMap displayName names)
  putStr (concatMap displayEdges edges)
  putStrLn "}"
  where
    names = listNames tree 1
    displayName (n, label, kind) = show n ++ "[label=" ++ show label ++ (if kind == 1 then ", shape=record" else "") ++ "]\n"
    edges = listEdges tree 1
    displayEdges (r, (c1, c2)) = show r ++ " -> {" ++ show c1 ++ "," ++ show c2++ "};\n"
    listNames (Node w (c1, c2)) n = (n, show w, 0):(listNames c1 (2*n) ++ listNames c2 (2*n+1))
    listNames (Leaf w char) n = [(n, '\'':char:"' - " ++ show w, 1)]
    listEdges (Node w (c1, c2)) n = (n, (2*n, 2*n+1)):(listEdges c1 (2*n) ++ listEdges c2 (2*n+1))
    listEdges (Leaf w char) n = []
