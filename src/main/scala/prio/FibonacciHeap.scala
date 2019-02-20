package prio

import scala.collection.mutable
import scala.collection.mutable.Map

class Node[A](var key: Int, val value: A, var children: List[Node[A]] = List(), var parent: Option[Node[A]] = None,
              var marked: Boolean = false) {
  def removeChild(node: Node[A]): Unit = {
    children = children.filter(child => child != node)
    node.parent = None
  }

  def addChild(node: Node[A]): Unit = {
    children = node :: children
    node.parent = Some(this)
  }

  def removeParent(): Unit = {
    parent.foreach(parent => parent.removeChild(this))
  }

  def degree: Int = children.length

  def allNodes: List[Node[A]] = this :: this.children.flatMap(_.allNodes)

  override def toString = s"Node($key, $children)"
}

class FibonacciHeap[A] extends HandleQueue[A,Node[A]] {
  var minPointer: Node[A] = _

  var trees: List[Node[A]] = List()

  def allHandles: List[Node[A]] = trees.flatMap(_.allNodes)

  def this(elems: (Int,A)*) = {
    this
    for((key,value) <- elems) insert(key,value)
  }

  override def insert(key: Int, elem: A): Node[A] = {
    val node = new Node[A](key, elem)
    trees = node :: trees
    if(minPointer != null && minPointer.key > node.key) {
      minPointer = node
    } else if(minPointer == null) {
      minPointer = node
    }
    node
  }

  override def minimum: Node[A] = minPointer

  override def deleteMin: Node[A] = {
    val min = minPointer
    if(minPointer != null) {
      val children = minPointer.children
      children.foreach(child => child.parent = None)
      trees = trees.filter(tree => tree != minPointer) ++ children
      consolidate()
      minPointer = null
      updateMinPointer()
    }
    min
  }

  private def consolidate(): Unit = {
    val degreeMap = mutable.Map.empty[Int, Node[A]]
    for(tree <- trees) {
      val degree = tree.degree
      if(degreeMap.get(degree).isEmpty) {
        degreeMap(degree) = tree
      } else {
        val ot = degreeMap(degree)
        if(ot.key < tree.key) {
          ot.addChild(tree)
        } else {
          tree.addChild(ot)
          degreeMap(degree) = tree
        }
      }
    }
    trees = degreeMap.values.toList
  }

  private def updateMinPointer(): Unit = {
    for(tree <- trees) {
      if(minPointer == null || tree.key < minPointer.key) {
        minPointer = tree
      }
    }
  }

  override def isEmpty: Boolean = trees.isEmpty

  override def toString: String = trees.toString()

  override def decreaseKey(handle: Node[A], newKey: Int): Node[A] = {
    handle.key = newKey
    handle.parent match {
      case Some(p) =>
        if(p.key < newKey) {
          handle
        } else {
          var parent = p
          var current = handle
          while(parent != null && parent.marked) {
            parent.removeChild(current)
            trees = current :: trees
            parent.marked = false
            current = parent
            parent = current.parent.orNull
          }
          if(parent != null) {
            parent.removeChild(current)
            trees = current :: trees
            parent.marked = true
          }
        }
      case None =>
    }

    updateMinPointer()
    handle
  }

  override def delete(handle: Node[A]): Unit = {
    decreaseKey(handle, Int.MinValue)
    deleteMin
  }
}
