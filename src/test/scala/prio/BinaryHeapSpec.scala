package prio

object BinaryHeapSpec extends PrioQueueSpec[BinaryHeap[Int], (Int,Int)]("Binary Heap") {
  override def build(elems: (Int, Int)*): BinaryHeap[Int] = new BinaryHeap[Int](elems:_*)

  override def key(handle: (Int, Int)): Int = handle._1
}