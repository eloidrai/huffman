module Huffman (createHuffman, getCodes, getCanonicalCodes, display, encode, decode) where

import Data.List (sort, sortBy, sortOn, insert)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.Bits (Bits(shift))


-- Tuple representing a given number of bits
newtype SizedBits = SizedBits (Integer, Int)

instance Show SizedBits where
  show (SizedBits (val, size)) = drop (length string - size) string
    where
      format = "%0" ++ show size ++ "b"
      string = printf format val

-- The tree datatype
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

getCodes :: Tree  -> [(Char, SizedBits)]
getCodes tree  = sortBy (\(a, SizedBits (va, la)) (b, SizedBits (vb, lb) ) -> if la /= lb then compare la lb else compare va vb) (codes tree $ SizedBits (0, 0))
  where
    codes (Leaf w v) code = [(v, code)]
    codes (Node w (c1, c2)) code = codes c1 (add0 code) ++ codes c2 (add1 code)
      where
        add0 (SizedBits (val, len)) = SizedBits (shift val 1, len+1)
        add1 (SizedBits (val, len)) = SizedBits (shift val 1 +1, len+1)

getCanonicalCodes :: Tree -> [(Char, SizedBits)]
getCanonicalCodes tree = reverse $ foldl f [] lengths
  where
    lengths :: [(Char, Int)]
    lengths = sortOn snd $ sortOn fst $  map (\(c, SizedBits (val, len)) -> (c, len)) (getCodes tree)
    f [] (char, size) = [(char, SizedBits (0, size))]
    f acc@((_, SizedBits (n, _)):_) (char, size) = (char, c) : acc
      where
        s = size - (floor.(+1).logBase 2) (fromIntegral (n+1))
        c = SizedBits (shift (n+1) s, size)

encode :: Tree -> String -> String
encode tree = concatMap (\c -> show $ fromMaybe (SizedBits (0, 0)) (lookup c dict))
  where
    dict = getCodes tree


decode :: Tree -> String -> String
decode tree string = fst $ foldl dec ([], "") string
  where
    dict = map  (\(char, bits) -> (show bits, char)) (getCodes tree)
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