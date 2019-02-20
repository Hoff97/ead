package prio

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop, Properties}

abstract class PrioQueueSpec[Q <: Queue[Int,H],H](name: String) extends Properties(name) {
  def build(elems: (Int,Int)*): Q
  def key(handle: H): Int

  property("buildMaintainsMin") = forAll { (a: List[Int]) =>
    if(a.nonEmpty) {
      val h = build(a.map(x => (x,x)):_*)

      key(h.minimum) == a.min
    } else {
      true
    }
  }

  property("deleteMinSorts") = forAll { (a: List[Int]) =>
    val h = build(a.map(x => (x,x)):_*)

    var s = List[Int]()
    for(j <- a.indices) s = key(h.deleteMin) :: s
    s.equals(a.sorted.reverse)

  }

  property("insertWorks") = forAll { (a: List[Int], b: List[Int]) =>
    val h = build(a.map(x => (x,x)):_*)
    b.foreach(x => h.insert(x,x))

    val items = a.length + b.length

    var s = List[Int]()
    for(j <- 0 until items) s = key(h.deleteMin) :: s
    s.equals((a ++ b).sorted.reverse)
  }

  val smallList: Gen[List[Int]] = Gen.listOf(Gen.choose(1,50))

  property("emptyCorrect") = Prop.forAll(smallList,smallList,Gen.choose(0,20)) { (a: List[Int], b: List[Int], remove: Int) =>
    if(remove > 0) {
      val h = build(a.map(x => (x,x)):_*)
      b.foreach(x => h.insert(x,x))

      val items = a.length + b.length

      var s = List[Int]()
      for(j <- 0 to Math.min(items - 1, remove - 1)) s = key(h.deleteMin) :: s
      if(h.isEmpty) {
        remove >= a.length+b.length
      } else {
        remove < a.length+b.length
      }
    } else {
      true
    }
  }

}