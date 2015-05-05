package bruno

import org.scalatest.{WordSpecLike, Matchers}


object P01 {

  def last[A](lst: List[A]) = lst.last

}

class P01Spec extends WordSpecLike with Matchers {

  "last" should {
    "return the last element of a List" in {
      P01.last(List(1, 1, 2, 3, 5, 8)) should be (8)
    }
  }

}
