package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Create a list containing all integers within a given range.
class P22Spec extends WordSpecLike with Matchers {


  "range" should {
    "return List(1) when the ranges are from 1 to 2" in {
      List.range(1, 2) shouldBe List(1)
    }
    "return List(1,2) when the ranges are from 1 to 3" in {
      List.range(1, 3) shouldBe List(1, 2)
    }
    "return List(4,5,6,7,8,9) when the ranges are from 4 to 10" in {
      List.range(4, 10) shouldBe List(4, 5, 6, 7, 8, 9)
    }
  }

  "range for comprehension" should {
    "return List(1) when the ranges are from 1 to 1" in {
      val l = for (i <- 1 to 1) yield i
      l shouldBe List(1)
    }
    "return List(1,2) when the ranges are from 1 to 2" in {
      val l = for (i <- 1 to 2) yield i
      l shouldBe List(1, 2)
    }
    "return List(4,5,6,7,8,9) when the ranges are from 4 to 9" in {
      val l = for (i <- 4 to 9) yield i
      l shouldBe List(4, 5, 6, 7, 8, 9)
    }
  }

}
