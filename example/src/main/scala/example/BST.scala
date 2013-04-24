package example

object BST {
  def apply() = Empty
}

trait BST {

  def contains(elem: Int): Boolean

  def add(elem: Int): BST
}

object Empty extends BST {

  def contains(elem: Int): Boolean = false

  def add(elem: Int): BST = new Node(elem, Empty, Empty)

  override def toString: String = "Empty"
}

case class Node(elem: Int, left: BST, right: BST) extends BST {

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

  override def toString: String = "Node( " + elem + ", " + left + ", " + right + ")"
}
