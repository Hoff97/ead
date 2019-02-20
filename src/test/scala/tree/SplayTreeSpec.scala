package tree

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop, Properties}

object SplayTreeSpec extends Properties("Splay Tree") {
  property("insertInOrderSorts") = forAll { (a: List[Int]) =>
    val t = new SplayTree[Int]
    val distinct = a.distinct

    distinct.foreach(x => t.insert(x,x))
    t.inOrder.map(x => x.key).equals(distinct.sorted)
  }

  property("deleteWorks1") = forAll { (a: List[Int], b: List[Int]) =>
    val t = new SplayTree[Int]
    val distinctA = a.distinct
    val distinctB = b.distinct.filter(x => !distinctA.contains(x))

    distinctA.foreach(x => t.insert(x,x))
    distinctB.foreach(x => t.insert(x,x))

    distinctB.foreach(x => t.delete(x))
    t.inOrder.map(x => x.key).equals(distinctA.sorted)
  }

  property("deleteWorks2") = forAll { (a: List[Int], b: List[Int]) =>
    val t = new SplayTree[Int]
    val distinctA = a.distinct
    val distinctB = b.distinct.filter(x => !distinctA.contains(x))

    distinctA.foreach(x => t.insert(x,x))
    distinctB.foreach(x => t.insert(x,x))

    distinctA.foreach(x => t.delete(x))
    t.inOrder.map(x => x.key).equals(distinctB.sorted)
  }
}
