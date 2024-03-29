module Main where

import Huffman (createHuffman, getCodes, getCanonicalCodes, display, encode, decode)

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Data.Maybe
import Data.List

optionsDefinition :: [OptDescr ([Char], String)]
optionsDefinition = [sample, help, tree, canonical, codewords, encode, decode]
  where
    sample = Option ['s'] ["sample"] (ReqArg (\s -> ("sample", s)) "SAMPLE")  "The set of symbols to use to create the tree"
    help = Option ['h'] ["help"] (NoArg ("help", ""))  "Displays this help message"
    tree = Option ['t'] ["tree", "dot"] (NoArg ("tree", ""))  "The tree in .dot format"
    canonical = Option ['a'] ["canonical"] (NoArg ("canonical", ""))  "Use the canonical Huffman code for generating the codewords, encoding and decoding"
    codewords = Option ['c'] ["codewords"] (NoArg ("codewords", ""))  "The list of the codewords"
    encode = Option ['e'] ["encode"] (ReqArg (\s -> ("encode", s)) "MESSAGE")  "Encodes a message"
    decode = Option ['d'] ["decode"] (ReqArg (\s -> ("decode", s)) "MESSAGE")  "Decodes a message"

displayHelp :: IO ()
displayHelp = putStr $ usageInfo "Huffman coding\n" optionsDefinition
printError = hPutStr stderr

main :: IO ()
main = do
  args <- getArgs
  let (options, nonOptions, errors) = getOpt Permute optionsDefinition args
  if isJust $ lookup "help" options then do
    displayHelp
    exitSuccess
  else if not (null errors) then do
    printError $ intercalate "\n" errors
    exitSuccess
  else if null nonOptions && isNothing (lookup "sample" options) then do
    printError "no option '-s' provided\n"
    exitSuccess
  else do
    let tree = createHuffman $ fromMaybe (head nonOptions) (lookup "sample" options)
        codes = (if isJust $ lookup "canonical" options then getCanonicalCodes tree else getCodes tree)
    if isJust $ lookup "tree" options then
      display tree
    else if isJust $ lookup "codewords" options then
      putStrLn $ intercalate "\n" $ map (\(char, code) -> char:" -> " ++ show code) codes
    else if isJust $ lookup "encode" options then
      putStrLn $ encode tree (fromMaybe "" (lookup "encode" options))
    else if isJust $ lookup "decode" options then do
      putStrLn $ decode tree (fromMaybe "" (lookup "decode" options))
    else do
      return ()