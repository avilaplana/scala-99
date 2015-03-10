package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.util.Random

//Lotto: Draw N different random numbers from the set 1..M.
//scala> lotto(6, 49)
//res0: List[Int] = List(23, 1, 17, 33, 21, 37)
class P24Spec extends WordSpecLike with Matchers {

  def lotto(n: Int, max: Int): List[Int] = {
    val r = Random
    @tailrec
    def random(max: Int, l: List[Int]): Int = {
      r.nextInt(max) match {
        case n if l.contains(n) => random(max, l)
        case n => n
      }
    }

    Range(1, n + 1).foldLeft(List.empty[Int])((l, e) => l :+ random(max, l))
  }

  "lotto" should {
    "return 1 number when the the set is between 1..10" in {
      val l = lotto(1, 10)
      l.filter(_ <= 10).toSeq.size shouldBe 1
    }
    "return 2 number when the the set is between 1..10" in {
      val l = lotto(2, 10)
      l.filter(_ <= 10).toSeq.size shouldBe 2
    }
    "return 10 number when the the set is between 1..10" in {
      val l = lotto(10, 10)
      l.filter(_ <= 10).toSeq.size shouldBe 10
    }
    "return 6 number when the the set is between 1..49" in {
      val l = lotto(6, 49)
      l.filter(_ <= 49).toSeq.size shouldBe 6
    }
  }
}
