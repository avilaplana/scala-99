package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P02 {
  def penultimate[A](lst: List[A]): A = lst.reverse.drop(1).head
}

class P02Spec extends WordSpecLike with Matchers {

  "penultimate" should {
    "return the last but one element of a list" in {
      P02.penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
    }
  }
}
