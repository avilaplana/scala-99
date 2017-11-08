package alvaro.codility.permMissingElem

import org.scalatest.{Matchers, WordSpec}

object Solution {
  def solution(a: Array[Int]): Int = {
    def find(a: List[Int], initialSize: Int): Int = a match {
        case 2 :: Nil if initialSize == 1 => 1
        case 1 :: Nil if initialSize == 1 => 2
        case e :: Nil if initialSize != 1 => e + 1
        case f :: s :: tail if (f + 1) == s => find(a.tail, initialSize)
        case f :: tail => f + 1
    }

    find(a.toList.sortWith((e1, e2) => e1 < e2), a.size)
  }
}

// array with 1 element can have 1 to 2
// array with 2 element can have 1 to 3
// array with 3 element can have 1 to 4
class PermMissingElemSpec extends WordSpec with Matchers {

  "solution" should {
    "return 1 when the array with size 1 contains 2" in {
      Solution.solution(Array(2)) shouldBe (1)
    }

    "return 2 when the array with size 1  contains 1" in {
      Solution.solution(Array(1)) shouldBe (2)
    }

    "return 3 when the array with size 2 contains 1,2" in {
      Solution.solution(Array(1, 2)) shouldBe (3)
    }

    "return 2 when the array with size 2 contains 1,3" in {
      Solution.solution(Array(1, 3)) shouldBe (2)
    }

    "return 4 when the array with size 3 contains 1,3,2" in {
      Solution.solution(Array(1, 3, 2)) shouldBe (4)
    }

    "return 4 when the array with size 4 contains 2,3,1,5" in {
      Solution.solution(Array(2, 3, 1, 5)) shouldBe (4)
    }


  }

}
