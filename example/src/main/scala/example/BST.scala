package example

object BST {
  def apply() = Empty
}

abstract class BST {
  abstract def contains(elem: Int): Boolean

  abstract def add(elem: Int): BST
}

object Empty extends BST {
  def contains(elem: Int): Boolean = false

  def add(elem: Int): BST = new Leaf(elem)
}

case class Leaf(elem: Int) extends BST{
  def contains(elem: Int): Boolean = {
    this.elem == elem
  }

  def add(elem: Int): BST = {
    if (elem < this.elem) new Node(this.elem, new Leaf(elem), Empty)
    else new Node(this.elem, Empty, new Leaf(elem))
  }
}

case class Node(elem: Int, left: BST, right: BST) extends BST{
  def contains(elem: Int): Boolean = {
    if (this.elem == elem) true
    else if (elem < this.elem) left.contains(elem)
    else right.contains(elem)
  }

  def add(elem: Int): BST = {
    if (this.elem == elem) this
    else if (elem < this.elem) new Node(this.elem, left.add(elem), right)
    else new Node(this.elem, left, right.add(elem))

  }
}
