package prio

import scala.annotation.tailrec

class BinaryHeap[A] extends Queue[A,(Int,A)] {
  var arr: Array[(Int,A)] = Array()
  var full: Int = 0

  def this(elems: (Int,A)*) = {
    this
    val els = Math.max(Math.pow(2,Math.ceil(Math.log(elems.length)/Math.log(2))),4).toInt
    arr = Array.ofDim(els)
    full = elems.length
    for(j <- 0 to (elems.length-1)) arr(j) = elems(j)
    for(j <- 0 to (elems.length-1)) siftDown(elems.length-1-j)
  }

  override def insert(key: Int, elem: A) = {
    if(full >= arr.length) {
      sizeUp
    }
    arr(full) = (key,elem)
    full = full+1
    siftUp(full-1)
    (key, elem)
  }

  override def minimum: (Int, A) = arr(0)

  override def deleteMin: (Int, A) = {
    val min = arr(0)
    if(full > 1) {
      val newRoot = arr(full-1)
      arr(0) = newRoot
      full = full-1
      siftDown(0)
    } else {
      full = full-1
    }
    min
  }

  override def isEmpty: Boolean = full == 0

  @tailrec
  private def siftDown(position: Int): Unit = {
    val elem = arr(position)
    val c1Ix = position*2+1
    val c2Ix = position*2+2
    val ch1 = if(c1Ix < full) arr(c1Ix)._1 else Int.MaxValue
    val ch2 = if(c2Ix < full) arr(c2Ix)._1 else Int.MaxValue
    if(elem._1 > Math.min(ch1,ch2)) {
      val swapIx = if(ch1 < ch2) c1Ix else c2Ix
      val swap = arr(swapIx)
      arr(position) = swap
      arr(swapIx) = elem
      siftDown(swapIx)
    }
  }

  @tailrec
  private def siftUp(position: Int): Unit = {
    if(position != 0) {
      val elem = arr(position)
      val parentIx = (position-1)/2
      val parent = arr(parentIx)
      if(elem._1 < parent._1) {
        arr(position) = parent
        arr(parentIx) = elem
        siftUp(parentIx)
      }
    }
  }

  private def sizeUp = {
    val newArr = Array.ofDim[(Int,A)](arr.length*2)
    for(j <- 0 to (full - 1)) newArr(j) = arr(j)
    arr = newArr
  }

  private def sizeDown = {
    val newArr = Array.ofDim[(Int,A)](arr.length/2)
    for(j <- 0 to (full - 1)) newArr(j) = arr(j)
    arr = newArr
  }
}
