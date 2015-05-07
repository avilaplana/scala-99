package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P09 {
  def pack[A](lst: List[A]): List[List[A]] = lst.foldRight(List.empty[List[A]]) {
    case (a, b) if b.headOption.exists(_.contains(a)) => (a :: b.head) :: b.tail
    case (a, b) => List(a) :: b
  }
}

class P09Spec extends WordSpecLike with Matchers {

  "pack" should {
    "pack consecutive duplicates of list elements into sublists" in {
      P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should contain theSameElementsInOrderAs
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }
}
