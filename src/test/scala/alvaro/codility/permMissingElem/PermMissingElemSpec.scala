package alvaro.codility.permMissingElem

import org.scalatest.{Matchers, WordSpecLike}

object Solution {
  def solution(A: Array[Int]): Int = {
    if (!Range(0, 100001).contains(A.size)) throw new IllegalArgumentException("Array size must be between 0 and 100001")


    def findMissing(head: Int, tail: Seq[Int]): Int = {
      tail match {
        case Nil => head + 1
        case t if head + 1 == t.head => findMissing(t.head, t.tail)
        case _ => head + 1
      }
    }

    val sorterd = A.sortWith((e1, e2) => e1 < e2)

    findMissing(sorterd.head, sorterd.tail)
  }
}

class PermMissingElemSpec extends WordSpecLike with Matchers {

  val eq = Array(2, 3, 1, 5)

  "the array 2,3,1,5" should {
    "have a minimal difference of 4" in {
      Solution.solution((eq)) shouldBe 4
    }
  }


}
