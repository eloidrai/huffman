module Main where

import Huffman (createHuffman, getCodes, display, encode)

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Data.Maybe
import Data.List

optionsDefinition = [sample, help, tree, codewords, encode, decode]
  where
    sample = Option ['s'] ["sample"] (ReqArg (\s -> ("sample", s)) "SAMPLE")  "The set of symbols to use to create the tree"
    help = Option ['h'] ["help"] (NoArg ("help", ""))  "Displays this help message"
    tree = Option ['t'] ["tree", "dot"] (NoArg ("tree", ""))  "The tree in .dot format"
    codewords = Option ['c'] ["codewords"] (NoArg ("codewords", ""))  "The list of the codewords"
    encode = Option ['e'] ["encode"] (ReqArg (\s -> ("encode", s)) "MESSAGE")  "Encodes a message"
    decode = Option ['d'] ["decode"] (ReqArg (\s -> ("decode", s)) "MESSAGE")  "Decodes a message"

displayHelp = putStr $ usageInfo "Huffman coding\n" optionsDefinition
printError = (hPutStr stderr)

main = do
  args <- getArgs
  let (options, nonOptions, errors) = (getOpt Permute optionsDefinition args)
  if (isJust $ lookup "help" options) then do
    displayHelp
    exitWith ExitSuccess
  else if (length errors /= 0) then do
    printError $ intercalate "\n" errors
    exitWith ExitSuccess
  else if ((length nonOptions == 0) && (isNothing $ lookup "sample" options)) then do
    printError "no option '-s' provided\n"
    exitWith ExitSuccess
  else do
    let tree = createHuffman $ maybe (head nonOptions) id (lookup "sample" options)
        codes = getCodes tree ""
    if (isJust $ lookup "tree" options) then do
      display tree
    else if (isJust $ lookup "codewords" options) then do
      putStrLn $ intercalate "\n" (map (\(char, code) -> char:" -> " ++ code) codes)
    else if (isJust $ lookup "encode" options) then do
      putStrLn $ encode tree (maybe "" id (lookup "encode" options))
    else if (isJust $ lookup "decode" options) then do
      putStr "decoded"
    else return ()
