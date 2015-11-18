package alvaro.codility.frogRiverOne

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object Solution {

  def solution(X: Int, A: Array[Int]): Int = {
    if (!(X >= 1 && X <= 100000) || !(A.size >= 1 && A.size <= 100000))
      throw new RuntimeException("X and N must be between 1 and 100000")

    @tailrec
    def shortest(X: Int, before: Set[Int], after: Array[Int], c: Int): Int = {
      if (after.isEmpty) {
        if (before.size == X) c + 1 else -1
      }  else {
        val co = before + after.head
        if (co.size == X) c + 1 else shortest(X, co, after.tail, c + 1)
      }
    }
    shortest(X, Set(A.head), A.tail, 0)
  }
}


class FrogRiverOneSpec extends WordSpecLike with Matchers {

  "the earliest time to get 5 for Array(1, 3, 1, 4, 2, 3, 5, 4)" should {
    "be 6" in {
      val p = Array(1, 1, 1, 5, 2, 4, 4, 5, 3, 5)
      Solution.solution(5, p) shouldBe 8
    }
  }

  "the earliest time to get 5 for Array(1, 3, 5, 4, 2, 3, 3, 5, 4)" should {
    "be 7" in {
      val p = Array(1, 3, 1, 4, 2, 3, 5, 4)
      Solution.solution(5, p) shouldBe 6
    }
  }

  "the earliest time to get 5 for Array(1, 3, 5, 2, 2, 3, 3, 5, 4)" should {
    "be -1" in {
      val p = Array(2, 3, 5, 2, 2, 3, 3, 5, 4)
      Solution.solution(5, p) shouldBe -1
    }
  }

  "the earliest time to get 1 for Array(1)" should {
    "be -1" in {
      val p = Array(1)
      Solution.solution(1, p) shouldBe 1
    }
  }



}
