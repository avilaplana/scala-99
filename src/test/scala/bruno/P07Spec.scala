package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P07 {
  def flatten[A](lst: List[_]): List[A] = lst.foldLeft(List.empty[A]) {
    case (b, as: List[_]) => b ++ flatten(as)
    case (b, a: A) => b :+ a
  }
}

class P07Spec extends WordSpecLike with Matchers {

  "flatten" should {
    "flatten a nested list structure" in {
      P07.flatten[Int](List(List(1, 1), 2, List(3, List(5, 8)))) should contain theSameElementsInOrderAs List(1, 1, 2, 3, 5, 8)
    }
  }
}
