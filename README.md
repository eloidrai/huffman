# The Huffman coding

## Usage
```
Huffman coding

  -s SAMPLE   --sample=SAMPLE   The set of symbols to use to create the tree
  -h          --help            Displays this help message
  -t          --tree, --dot     The tree in .dot format
  -c          --codewords       The list of the codewords
  -e MESSAGE  --encode=MESSAGE  Encodes a message
  -d MESSAGE  --decode=MESSAGE  Decodes a message
```

## Printing Huffman trees

This executable allows you to print the representation of the binary tree using the `dot` syntax.

Example : `huffman -s "this is a huffman tree" -t | dot -Tpng -o tree.png`

![The tree](tree.png)

## Encoding/decoding example

Encoding : ```sh
$ huffman -s "coucoutoi" -e "touco"
01111100011
```
Decoding : ```sh
$ huffman -s "coucoutoi" -d "01111100011"
touco
```