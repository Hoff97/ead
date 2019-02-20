package prio

trait Queue[A,Handle] {
  def insert(key: Int, elem: A): Handle
  def minimum: Handle
  def deleteMin: Handle
  def isEmpty: Boolean
}

trait HandleQueue[A,Handle] extends Queue[A,Handle] {
  def decreaseKey(handle: Handle, newKey: Int): Handle
  def delete(handle: Handle)
  def allHandles: List[Handle]
}
