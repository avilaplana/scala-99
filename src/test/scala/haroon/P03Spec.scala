package haroon

import org.scalatest.{Matchers, WordSpec}

object P03 {

  def nth(index: Int, list: List[Any]) : Any = {
    list.lift(index).getOrElse(None)
  }
}

class P03Spec extends WordSpec with Matchers {

  "P03 Spec" should {

    "return none on empty list" in {
      P03.nth(1, List()) shouldBe None
    }

    "return none if nth is greater than the length of the list" in {
      P03.nth(4, List(1,2,3)) shouldBe None
    }

    "return last element of the list if n is the same as the length of the list" in {
      P03.nth(2, List(1,2,3)) shouldBe 3
    }

    "return first element if list has only 1 element and n is 1" in {
      P03.nth(0, List(1)) shouldBe 1
    }

    "return nth element from the list" in {
      P03.nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
    }
  }
}