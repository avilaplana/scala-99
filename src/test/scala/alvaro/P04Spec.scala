package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object P04 {
  def length(list: List[Int]): Int = {
    @tailrec
    def calculateLength(l: Int, pl: List[Int]): Int =
      pl match {
        case Nil => l
        case head :: tail => calculateLength(l + 1, tail)
      }
    calculateLength(0, list)
  }
}

class P04Spec extends WordSpecLike with Matchers {

  "length" should {
    "return 0 when the list is empty" in {
      P04.length(Nil) shouldBe 0
    }
    "return 1 when the list is 5" in {
      P04.length(List(5)) shouldBe 1
    }
    "return 3 when the list is 5,3,1" in {
      P04.length(List(5, 3, 1)) shouldBe 3
    }
    "return 5 when the list is 5,4,3,2,1" in {
      P04.length(List(5, 4, 3, 2, 1)) shouldBe 5
    }
  }

}
