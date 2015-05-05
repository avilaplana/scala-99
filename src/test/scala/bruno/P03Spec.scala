package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P03 {
  def nth[A](n: Int, lst: List[A]): A = lst(n)
}

class P03Spec extends WordSpecLike with Matchers {

  "nth" should {
    "find the Kth element of a list" in {
      P03.nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
    }
  }
}
