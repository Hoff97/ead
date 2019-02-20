package prio

object FibonacciHeapSpec extends HandleQueueSpec[FibonacciHeap[Int], Node[Int]]("Fibonacci Heap") {
  override def build(elems: (Int, Int)*): FibonacciHeap[Int] = new FibonacciHeap[Int](elems:_*)

  override def key(handle: Node[Int]): Int = handle.key
}