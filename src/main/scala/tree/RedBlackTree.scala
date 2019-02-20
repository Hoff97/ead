package tree

import scala.annotation.tailrec

object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}

class RedBlackNode[A](var key: Int, val value: A, var left: Option[RedBlackNode[A]] = None, var right: Option[RedBlackNode[A]] = None,
                      var parent: Option[RedBlackNode[A]] = None, var color: Color.Color = Color.Red) {
  def degree: Int = left.map(_ => 1).getOrElse(0) + right.map(_ => 1).getOrElse(1)

  def preOrder: List[RedBlackNode[A]] =
    List(this) ++ this.left.map(_.preOrder).getOrElse(List()) ++ this.right.map(_.preOrder).getOrElse(List())

  def inOrder: List[RedBlackNode[A]] =
    this.left.map(_.inOrder).getOrElse(List()) ++ List(this) ++ this.right.map(_.inOrder).getOrElse(List())

  def postOrder: List[RedBlackNode[A]] =
    this.left.map(_.postOrder).getOrElse(List()) ++ this.right.map(_.postOrder).getOrElse(List()) ++ List(this)

  def find(key: Int): RedBlackNode[A] = {
    if (key == this.key) {
      this
    } else {
      if (key <= this.key) {
        this.left.map(_.find(key)).getOrElse(this)
      } else {
        this.right.map(_.find(key)).getOrElse(this)
      }
    }
  }

  def max: RedBlackNode[A] = this.right.map(_.max).getOrElse(this)

  def min: RedBlackNode[A] = this.left.map(_.min).getOrElse(this)

  def insert(key: Int, value: A): RedBlackNode[A] = {
    val n = find(key)
    val node = new RedBlackNode[A](key, value, None, None, Some(n))
    if(n.key >= key) {
      n.left = Some(node)
    } else {
      n.right = Some(node)
    }
    node.fixupInsert()
    node
  }

  @tailrec
  final def fixupInsert(): Unit = {
    if(parent.isEmpty) {
      this.color = Color.Black
    } else {
      val parent = this.parent.get
      if(parent.color == Color.Red) {
        if(parent.parent.isEmpty) {
          parent.color = Color.Black
        } else {
          val gp = parent.parent.get
          val pL = parent.isLeftChild
          val uncle = if(pL) gp.right else gp.left

          if(uncle.isDefined && uncle.get.color == Color.Red) {             //Case 1
            parent.color = Color.Black
            uncle.get.color = Color.Black
            gp.color = Color.Red
            gp.fixupInsert()
          } else if(isCase2b(this, parent, gp, uncle)) {                    //Case 2b
            if(isLeftChild) gp.rightRotate() else gp.leftRotate()
            parent.color = Color.Black
            gp.color = Color.Red
          } else if(isCase2a(this, parent, gp, uncle)) {                    //Case 2a
            if(isRightChild) parent.leftRotate() else parent.rightRotate()
            parent.fixupInsert()
          }
        }
      }
    }
  }

  protected def isCase2b(z: RedBlackNode[A], parent: RedBlackNode[A], gp: RedBlackNode[A], uncle: Option[RedBlackNode[A]]): Boolean = {
    ((uncle.isDefined && uncle.get.color == Color.Black) || uncle.isEmpty) &&
      ((z.isLeftChild && parent.isLeftChild) || (z.isRightChild && parent.isRightChild))
  }

  protected def isCase2a(z: RedBlackNode[A], parent: RedBlackNode[A], gp: RedBlackNode[A], uncle: Option[RedBlackNode[A]]): Boolean = {
    ((uncle.isDefined && uncle.get.color == Color.Black) || uncle.isEmpty) &&
      ((z.isLeftChild && parent.isRightChild) || (z.isRightChild && parent.isLeftChild))
  }

  def delete(): Unit = {
    var r: Option[RedBlackNode[A]] = None
    var succ: Option[RedBlackNode[A]] = None
    var p: Option[RedBlackNode[A]] = None
    if(this.right.isDefined) {
      val m = this.right.get.min
      val r = Some(m)
      succ = m.right
      p = m.parent
      m.spliceOut()
    } else if(this.left.isDefined) {
      val m = this.left.get.max
      val r = Some(m)
      succ = m.left
      p = m.parent
      m.spliceOut()
    }

    r match {
      case Some(rep) => {
        rep.left = this.left
        rep.left.foreach(_.parent = Some(rep))

        rep.right = this.right
        rep.right.foreach(_.parent = Some(rep))

        val oldColor = rep.color

        val lc = this.isLeftChild
        rep.parent = this.parent
        if(lc) rep.parent.foreach(_.left = Some(rep)) else rep.parent.foreach(_.right = Some(rep))
        rep.color = this.color

        if(oldColor == Color.Black) {
          fixupDelete(succ, p)
        }
      }
      case None => {
        // TODO: Probably done?
      }
    }
  }

  protected def fixupDelete(z: Option[RedBlackNode[A]], parent: Option[RedBlackNode[A]]): Unit = {
    if(z.isDefined && z.get.color == Color.Red) {
      z.get.color = Color.Black
    } else if (parent.isDefined) {
      val p = parent.get
      //TODO
    }
  }

  def spliceOut(): Unit = {
    if(this.degree == 2) {
      throw new RuntimeException("Cant splice out element with two children")
    } else {
      val child = if(this.right.isDefined) this.right else this.left
      val parent = this.parent
      if(this.isLeftChild) {
        this.parent.foreach(_.left = child)
      } else {
        this.parent.foreach(_.right = child)
      }
      child.foreach(_.parent = parent)
    }
  }

  protected def isLeftChild: Boolean = this.parent.isDefined && this.parent.get.left.contains(this)

  protected def isRightChild: Boolean = this.parent.isDefined && this.parent.get.right.contains(this)

  protected def leftRotate(): Unit = {
    val parent = this.parent
    val r = this.right
    val rchl = this.right.flatMap(_.left)

    this.right = rchl
    rchl.foreach(_.parent = Some(this))

    this.parent = r
    r.foreach(_.left = Some(this))

    r.foreach(_.parent = parent)
    parent.foreach{ p =>
      if(p.left.contains(this)) {
        p.left = r
      } else {
        p.right = r
      }
    }
  }

  protected def rightRotate(): Unit = {
    val parent = this.parent
    val l = this.left
    val lchr = this.left.flatMap(_.right)

    this.left = lchr
    lchr.foreach(_.parent = Some(this))

    this.parent = l
    l.foreach(_.right = Some(this))

    l.foreach(_.parent = parent)
    parent.foreach{ p =>
      if(p.left.contains(this)) {
        p.left = l
      } else {
        p.right = l
      }
    }
  }

  @tailrec
  final def root: RedBlackNode[A] = parent match {
    case Some(p) => p.root
    case None => this
  }

  override def toString = s"Node($key, $left, $right)"
}

class RedBlackTree[A] {
  var root: Option[RedBlackNode[A]] = None

  def insert(key: Int, value: A): RedBlackNode[A] = this.root match {
    case Some(r) =>
      val n = r.insert(key, value)
      this.root = Some(n.root)
      n
    case None =>
      val node = new RedBlackNode[A](key, value)
      this.root = Some(node)
      node
  }

  def inOrder: List[RedBlackNode[A]] = this.root.map(_.inOrder).getOrElse(List())
  def preOrder: List[RedBlackNode[A]] = this.root.map(_.preOrder).getOrElse(List())
  def postOrder: List[RedBlackNode[A]] = this.root.map(_.postOrder).getOrElse(List())
}
