package alvaro.codility.missingInteger

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object Solution {
  def solution(a: Array[Int]): Int = {
    val setSorted: Seq[Int] = a.toSet.toSeq.sortWith((b1, b2) => b1 < b2).filter(_ > 0)

    @tailrec
    def minimalPositive(b:Int, af: Seq[Int], candidate: Int): Int ={
      af match {
        case e +: tail if (b + 1) == e => minimalPositive(e, tail, candidate)
        case e +: tail  if (b < 0 && e == 1) => minimalPositive(e, tail, candidate)
        case e +: _  if (b < 0 && e > 1) => 1
        case e +: _  if (b > 0) => b + 1
        case _ =>  throw new RuntimeException("unexpected case")
      }
    }
    minimalPositive(setSorted.head, setSorted.tail, setSorted.head)
  }
}

class MissIntegerspec extends WordSpecLike with Matchers {

  "minimal positive integer (greater than 0)" should {
    "be 5 when the list is 1,3,6,4,1,2" in {
      Solution.solution(Array(1,3,6,4,1,2)) shouldBe 5
    }

    "be 4 when the list is 1,3,6,7,1,2" in {
      Solution.solution(Array(1,3,6,7,1,2)) shouldBe 4
    }

    "be 2 when the list is 1,3" in {
      Solution.solution(Array(1,3)) shouldBe 2
    }

    "be 2 when the list is 1,4" in {
      Solution.solution(Array(1,4)) shouldBe 2
    }

    "be 1 when the list is -1,4" in {
      Solution.solution(Array(-1,4)) shouldBe 1
    }

    "be 2 when the list is -1,1,4" in {
      Solution.solution(Array(-1,1,4)) shouldBe 2
    }
  }

}
