package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P04 {
  def length[A](lst: List[A]): Int = lst.length
}

class P04Spec extends WordSpecLike with Matchers {

  "length" should {
    "find the length of a list" in {
      P04.length(List(1, 1, 2, 3, 5, 8)) should be (6)
    }
  }
}
