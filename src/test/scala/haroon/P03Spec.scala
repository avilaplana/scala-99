package haroon

import haroon.P03._
import org.scalatest.{Matchers, WordSpec}

class P03Spec extends WordSpec with Matchers {

  "P03 Spec" should {

    "return none on empty list" in {
      nth(1, List()) shouldBe None
    }

    "return none if nth is greater than the length of the list" in {
      nth(4, List(1,2,3)) shouldBe None
    }

    "return last element of the list if n is the same as the length of the list" in {
      nth(2, List(1,2,3)) shouldBe 3
    }

    "return first element if list has only 1 element and n is 1" in {
      nth(0, List(1)) shouldBe 1
    }

    "return nth element from the list" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
    }
  }
}