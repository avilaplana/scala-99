package haroon


import org.scalatest.{WordSpec, Matchers}

class P04Spec extends WordSpec with Matchers {

  "P04 Spec" should {

    "return 0 on empty list" in {
      P04.length(List()) shouldBe 0
    }

    "return 0 on nil list" in {
      P04.length(Nil) shouldBe 0
    }

    "return 1 for a list with one element" in {
      P04.length(List(2)) shouldBe 1

    }

    "return 6 for 6 elements in the list" in {
      P04.length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
    }

    "return 5 for 5 elements in the list" in {
      P04.length(List("bar",3,4,5,"foo")) shouldBe 5
    }
  }
}

