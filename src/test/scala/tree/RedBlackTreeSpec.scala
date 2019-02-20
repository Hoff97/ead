package tree

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop, Properties}

object RedBlackTreeSpec extends Properties("RedBlack Tree") {
  property("insertInOrderSorts") = forAll { (a: List[Int]) =>
    val t = new SplayTree[Int]
    val distinct = a.distinct

    distinct.foreach(x => t.insert(x,x))
    t.inOrder.map(x => x.key).equals(distinct.sorted)
  }
}
