package haroon

import org.scalatest.{Matchers, WordSpec}

class P05Spec extends WordSpec with Matchers {

  "P05 Spec" should {

    "return empty list on empty list" in {
      P05.reverse(List()) shouldBe List()
    }

    "return the same list if there is only one element" in {
      val list = List(2)
      P05.reverse(List(2)) shouldBe list

    }

    "return the reverse list for more than 1 element in the list" in {
      val list = List(1, 1, 2, 3, 5, 8)
      P05.reverse(list) shouldBe list.reverse
    }
  }
}