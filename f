digraph g {labelloc="t"
label="Arbre de Huffman"
1[label="12"]
2[label="5"]
4[label="t - 2", shape=record]
5[label="e - 3", shape=record]
3[label="7"]
6[label="3"]
12[label="o - 1", shape=record]
13[label="2"]
26[label=". - 1"]
27[label="i - 1", shape=record]
7[label="4"]
14[label="m - 2", shape=record]
15[label="p - 2", shape=record]
1 -> {2,3};
2 -> {4,5};
3 -> {6,7};
6 -> {12,13};
13 -> {26,27};
7 -> {14,15};
}
