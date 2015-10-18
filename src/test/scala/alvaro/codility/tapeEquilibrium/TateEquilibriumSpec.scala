package alvaro.codility.tapeEquilibrium

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object Solution {
  def solution(A: Array[Int]): Int = {
    if (!Range(2, 100000).contains(A.size)) throw new IllegalArgumentException("Array size must be between 2 and 100000")

    @tailrec
    def minDifference(h: Seq[Int], t: Seq[Int], accumulatedDiff: Int): Int = {
      (h,t) match {
        case (_,Nil) => accumulatedDiff
        case _ =>
          val diff = Math.abs(h.sum - t.sum)
          val min = if (diff < accumulatedDiff) diff else accumulatedDiff
          minDifference(h :+ t.head, t.tail, min)
      }
    }
    minDifference(Seq(A.head), A.tail.toSeq, Int.MaxValue)
  }
}


class TateEquilibriumSpec extends WordSpecLike with Matchers {

  val eq = Array(3, 1, 2, 4, 3)

  "the array 3,1,2,4,3" should {
    "have a minimal difference of 1" in {
      Solution.solution((eq)) shouldBe 1
    }
  }

  "empty array" should {
    "raise an exception" in {
      an [IllegalArgumentException] should be thrownBy Solution.solution((Array()))
    }
  }

}
