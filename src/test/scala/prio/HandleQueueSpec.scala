package prio

import org.scalacheck.Prop

abstract class HandleQueueSpec[Q <: HandleQueue[Int,H],H](name: String) extends PrioQueueSpec[Q,H](name) {
  property("deleteWorks1") = Prop.forAll(smallList,smallList) { (a: List[Int], b: List[Int]) =>
    val h = build(a.map(x => (x,x)):_*)
    val handles = b.map(x => h.insert(x,x))

    handles.foreach(handle => h.delete(handle))

    var s = List[Int]()
    while(!h.isEmpty) {
      s = key(h.deleteMin) :: s
    }
    s.equals(a.sorted.reverse)
  }

  property("deleteWorks2") = Prop.forAll(smallList,smallList) { (a: List[Int], b: List[Int]) =>
    val h = build(a.map(x => (x,x)):_*)
    val handles = h.allHandles
    b.map(x => h.insert(x,x))

    handles.foreach(handle => h.delete(handle))

    var s = List[Int]()
    while(!h.isEmpty) {
      s = key(h.deleteMin) :: s
    }
    s.equals(b.sorted.reverse)
  }
}
