package example

import scala.annotation.tailrec

object BST {
  def apply(): BST = Empty

  def apply(ints : Int*): BST = ints.foldLeft(BST()) {_ ++ _}

}

trait BST {

  def contains(elem: Int): Boolean

  def add(elem: Int): BST

  def remove(elem: Int): BST

  def traverse(f: Int => Unit) = traverseInfix(f)

  def traversePrefix(implicit f: (Int => Unit)): Unit = {
    this match {
      case Leaf(e) => f(e)
      case Node(e, l, r) =>
        l.traversePrefix
        f(e)
        r.traversePrefix
      case _ =>
    }
  }

  def traverseInfix(implicit f: Int => Unit): Unit = {
    this match {
      case Leaf(e) => f(e)
      case Node(e, l, r) =>
        f(e)
        l.traversePrefix
        r.traversePrefix
      case _ =>
    }
  }

  def traversePostfix(implicit f: Int => Unit) = {
    this match {
      case Leaf(e) => f(e)
      case Node(e, l, r) =>
        l.traversePrefix
        r.traversePrefix
        f(e)
      case _ =>
    }
  }

  final def ++(elem: Int): BST = add(elem)

  final def --(elem: Int): BST = remove(elem)
}

private object Empty extends BST {

  def contains(elem: Int): Boolean = false

  def add(elem: Int): BST = new Leaf(elem)

  def remove(elem: Int): BST = this

  override def toString: String = "Empty"

}

private case class Leaf(elem: Int) extends BST {

  def contains(elem: Int): Boolean = this.elem == elem

  def add(elem: Int): BST = {
    if (elem == this.elem) this
    else if (elem < this.elem) new Node(this.elem, new Leaf(elem), Empty)
    else new Node(this.elem, Empty, new Leaf(elem))
  }

  def remove(elem: Int): BST = {
    if (elem == this.elem) Empty
    else this
  }

  override def toString: String = "Leaf(" + elem + ")"
}

private case class Node(elem: Int, left: BST, right: BST) extends BST {

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

  def remove(elem: Int): BST = {
    if (this.elem == elem) {
      val l = left match {
        case Empty => right
        case Leaf(e) => new Node(e, Empty, right)
        case Node(e, l, r) => new Node(e, l, ???)
      }

      l
    }
    else if (elem < this.elem) new Node(this.elem, left.remove(elem), right)
    else new Node(this.elem, left, right.remove(elem))
  }

  override def toString: String = "Node(" + elem + ", " + left + ", " + right + ")"
}
