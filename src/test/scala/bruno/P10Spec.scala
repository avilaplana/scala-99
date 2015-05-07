package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P10 {
  def encode[A](lst: List[A]): List[(Int, A)] = lst.foldRight(List.empty[(Int, A)]) {
    case (a, b) if b.headOption.exists(_._2 == a) => (b.head._1 + 1, b.head._2) :: b.tail
    case (a, b) => (1, a) :: b
  }
}

class P10Spec extends WordSpecLike with Matchers {

  "encode" should {
    "run-length encoding of a list" in {
      P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should contain theSameElementsInOrderAs
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }
}
