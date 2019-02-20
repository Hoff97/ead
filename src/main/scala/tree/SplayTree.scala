package tree

class SplayNode[A](var key: Int, val value: A, var left: Option[SplayNode[A]] = None, var right: Option[SplayNode[A]] = None,
                   var parent: Option[SplayNode[A]] = None) {
  def removeChild(node: SplayNode[A]): Unit = {
    if(left.isDefined && left.get == node) {
      left = None
    } else if(right.isDefined && right.get == node) {
      left = None
    }
    node.parent = None
  }

  def removeParent(): Unit = {
    parent.foreach(parent => parent.removeChild(this))
  }

  def degree: Int = left.map(_ => 1).getOrElse(0) + right.map(_ => 1).getOrElse(1)

  def allNodes: List[SplayNode[A]] =
    this.left.map(_.allNodes).getOrElse(List()) ++ List(this) ++ this.right.map(_.allNodes).getOrElse(List())

  def find(key: Int): SplayNode[A] = {
    if(key == this.key) {
      this
    } else {
      if(key <= this.key) {
        this.left.map(_.find(key)).getOrElse(this)
      } else {
        this.right.map(_.find(key)).getOrElse(this)
      }
    }
  }

  def max: SplayNode[A] = this.right.map(_.max).getOrElse(this)
  def min: SplayNode[A] = this.left.map(_.min).getOrElse(this)

  protected def isLeftChild: Boolean = this.parent.isDefined && this.parent.get.left.contains(this)
  protected def isRightChild: Boolean = this.parent.isDefined && this.parent.get.right.contains(this)

  protected def rotate(): Unit = {
    val parent = this.parent.get
    val gp = parent.parent

    if(this.isLeftChild) {
      val r = this.right
      parent.left = r
      r.foreach{r =>
        r.parent = Some(parent)
      }
      this.right = Some(parent)
    } else if(this.isRightChild) {
      val l = this.left
      parent.right = l
      l.foreach{l =>
        l.parent = Some(parent)
      }
      this.left = Some(parent)
    }

    gp.foreach(x => {
      if(x.left.contains(parent)) {
        x.left = Some(this)
      } else {
        x.right = Some(this)
      }
    })
    parent.parent = Some(this)
    this.parent = gp
  }

  def splay(): Unit = {
    if(this.parent.isDefined) {
      val parent = this.parent.get
      if(parent.parent.isEmpty) {                                                                         //Zig case
        rotate()
      } else if((this.isRightChild && parent.isLeftChild) || (this.isLeftChild && parent.isRightChild)) { //Zig-zag case
        rotate()
        rotate()
        splay()
      } else {                                                                                            //Zig-zig case
        val grandparent = parent.parent.get
        if(this.isLeftChild) {
          val r = this.right
          val pr = parent.right

          this.parent = grandparent.parent
          this.parent.foreach(x => {
            if(x.left.isDefined && x.left.get == grandparent) {
              x.left = Some(this)
            } else {
              x.right = Some(this)
            }
          })
          this.right = Some(parent)

          parent.parent = Some(this)
          parent.left = r
          parent.left.foreach(l => l.parent = Some(parent))
          parent.right = Some(grandparent)

          grandparent.parent = Some(parent)
          grandparent.left = pr
          grandparent.left.foreach(l => l.parent = Some(grandparent))
        } else {
          val l = this.left
          val pl = parent.left

          this.parent = grandparent.parent
          this.parent.foreach(x => {
            if(x.left.isDefined && x.left.get == grandparent) {
              x.left = Some(this)
            } else {
              x.right = Some(this)
            }
          })
          this.left = Some(parent)

          parent.parent = Some(this)
          parent.right = l
          parent.right.foreach(l => l.parent = Some(parent))
          parent.left = Some(grandparent)

          grandparent.parent = Some(parent)
          grandparent.right = pl
          grandparent.right.foreach(l => l.parent = Some(grandparent))
        }
        splay()
      }
    }
  }

  override def toString = s"Node($key, $left, $right)"
}

class SplayTree[A] {
  var root: Option[SplayNode[A]] = None

  def find(key: Int): Option[SplayNode[A]] = this.root.flatMap { root =>
    val n = root.find(key)
    n.splay()
    this.root = Some(n)
    if(n.key == key) Some(n)
    else None
  }

  def insert(key: Int, value: A): SplayNode[A] = this.root match {
    case Some(r) =>
      val n = r.find(key)
      n.splay()
      val node = new SplayNode[A](key, value)
      if(n.key <= key) {
        n.parent = Some(node)
        node.right = n.right
        node.right.foreach(x => x.parent = Some(node))
        node.left = Some(n)
        n.right = None
      } else {
        n.parent = Some(node)
        node.left = n.left
        node.left.foreach(x => x.parent = Some(node))
        node.right = Some(n)
        n.left = None
      }
      this.root = Some(node)
      node
    case None =>
      val node = new SplayNode[A](key, value)
      this.root = Some(node)
      node
  }

  def delete(key: Int): Unit = this.root.foreach { root =>
    val n = root.find(key)
    n.splay()
    this.root = Some(n)
    if(n.key == key) {
      n.right.foreach(x => x.parent = None)
      n.left.foreach(x => x.parent = None)

      (n.left,n.right) match {
        case (Some(l),r) =>
          val m = l.max
          m.splay()
          m.right = r
          r.foreach(x => x.parent = Some(m))
          this.root = Some(m)
        case (l, Some(r)) =>
          val m = r.min
          m.splay()
          m.left = l
          l.foreach(x => x.parent = Some(m))
          this.root = Some(m)
        case _ => this.root = None
      }
    }
  }

  def inOrder: List[SplayNode[A]] = this.root.map(_.allNodes).getOrElse(List())

  override def toString: String = this.root match {
    case Some(r) => s"Tree($r)"
    case None => "Tree()"
  }
}
