/**
 * Created with IntelliJ IDEA.
 * User: proximo
 * Date: 4/16/13
 * Time: 11:35 PM
 */

import patmat.Huffman._
times(List('a', 'b', 'a', 'c', 'c', 'c', 'd', 'd', 'c'))
//decodedSecret

val leaves = List(Leaf('a', 8), Leaf('b', 3), Leaf('c', 1), Leaf('d', 1),
  Leaf('e', 1), Leaf('f', 1), Leaf('g', 1), Leaf('h', 1))

val codeTree = until(singleton, combine) (leaves)



decode(codeTree.head, List(1011))

















































