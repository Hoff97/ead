package prio

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

class BinomialNode[A](var key: Int, val value: A, var children: List[BinomialNode[A]] = List(), var parent: Option[BinomialNode[A]] = None) {
  def removeChild(node: BinomialNode[A]): Unit = {
    children = children.filter(child => child != node)
    node.parent = None
  }

  def addChild(node: BinomialNode[A]): Unit = {
    children = node :: children
    node.parent = Some(this)
  }

  def removeParent(): Unit = {
    parent.foreach(parent => parent.removeChild(this))
  }

  def degree: Int = children.length

  def allNodes: List[BinomialNode[A]] = this :: this.children.flatMap(_.allNodes)

  @tailrec
  final def bubbleUp(): Unit = parent match {
    case Some(p) =>
      if(p.key > key) {
        this.parent = p.parent

        val childs = children
        val parentChilds = p :: p.children.filter(child => child != this)
        childs.foreach(x => x.parent = Some(p))
        p.children = childs

        parentChilds.foreach(x => x.parent = Some(this))
        this.children = parentChilds
        this.parent.foreach(x => x.removeChild(p))
        this.parent.foreach(x => x.addChild(this))

        bubbleUp()
      }
    case None =>
  }

  def merge(node: BinomialNode[A]): BinomialNode[A] = {
    if(node.key < key) {
      node.merge(this)
    } else {
      children = node :: children
      node.parent = Some(this)
      this
    }
  }

  override def toString = s"Node($key, $children)"
}

class BinomialHeap[A] extends HandleQueue[A,BinomialNode[A]] {
  var minIx: Int = -1

  var trees: mutable.Map[Int,BinomialNode[A]] = mutable.Map.empty
  var maxDegree: Int = -1

  def this(elems: (Int,A)*) = {
    this
    for((key,value) <- elems) insert(key,value)
  }

  override def insert(key: Int, elem: A): BinomialNode[A] = {
    val m: mutable.Map[Int,BinomialNode[A]] = mutable.Map.empty
    val node = new BinomialNode(key, elem)
    m(1) = node
    merge(m, 1)
    node
  }

  override def minimum: BinomialNode[A] = trees(minIx)

  override def deleteMin: BinomialNode[A] = {
    val min = minimum
    val m: mutable.Map[Int,BinomialNode[A]] = mutable.Map.empty
    for(tree <- min.children) {
      tree.parent = None
      m(tree.degree+1) = tree
    }
    trees.remove(minIx)
    merge(m, min.children.length)
    min
  }

  override def isEmpty: Boolean = trees.isEmpty

  private def merge(trees: mutable.Map[Int,BinomialNode[A]], maxDegree: Int): Unit = {
    var carry: BinomialNode[A] = null
    val m = Math.max(this.maxDegree,maxDegree)

    var minIx = -1
    var minKey: Int = Int.MaxValue

    for(i <- 1 to m) {
      val res = add(this.trees.getOrElse(i, null), trees.getOrElse(i,null), carry)
      if(res._1 != null) {
        this.trees(i) = res._1
        if(minKey == Int.MaxValue || minKey >= res._1.key) {
          minIx = i
          minKey = res._1.key
        }
      } else {
        this.trees.remove(i)
      }
      carry = res._2
    }
    if(carry != null) {
      this.trees(m+1) = carry
      this.maxDegree = m+1
      if(minKey == Int.MaxValue || minKey >= carry.key) {
        minIx = m+1
        minKey = carry.key
      }
    }

    this.minIx = minIx
  }

  private def add(a: BinomialNode[A], b: BinomialNode[A], carry: BinomialNode[A]): (BinomialNode[A], BinomialNode[A]) = {
    add((if(a == null) List() else List(a)) ++ (if(b == null) List() else List(b)) ++ (if(carry == null) List() else List(carry)))
  }

  private def add(n: List[BinomialNode[A]]): (BinomialNode[A], BinomialNode[A]) = n match {
    case List(a,b,c) => (a,b.merge(c))
    case List(a,b) => (null,a.merge(b))
    case List(a) => (a,null)
    case _ => (null,null)
  }

  override def toString = s"BH($trees, $minIx)"

  override def decreaseKey(handle: BinomialNode[A], newKey: Int): BinomialNode[A] = {
    handle.key = newKey
    handle.bubbleUp()
    if(handle.parent.isEmpty) {
      trees(handle.degree+1) = handle
      if(handle.key <= trees(minIx).key) {
        minIx = handle.degree+1
      }
    }
    handle
  }

  override def delete(handle: BinomialNode[A]): Unit = {
    decreaseKey(handle, Int.MinValue)
    deleteMin
  }

  override def allHandles: List[BinomialNode[A]] = this.trees.values.flatMap(_.allNodes).toList
}
