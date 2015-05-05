package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P05 {
  def reverse[A](lst: List[A]): List[A] = lst.reverse
}

class P05Spec extends WordSpecLike with Matchers {

  "reverse" should {
    "invert a list" in {
      P05.reverse(List(1, 1, 2, 3, 5, 8)) should contain theSameElementsInOrderAs List(8, 5, 3, 2, 1, 1)
    }
  }
}
