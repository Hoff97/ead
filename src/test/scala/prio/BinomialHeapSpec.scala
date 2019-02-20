package prio

object BinomialHeapSpec extends HandleQueueSpec[BinomialHeap[Int], BinomialNode[Int]]("Binomial Heap") {
  override def build(elems: (Int, Int)*): BinomialHeap[Int] = new BinomialHeap[Int](elems:_*)

  override def key(handle: BinomialNode[Int]): Int = handle.key
}