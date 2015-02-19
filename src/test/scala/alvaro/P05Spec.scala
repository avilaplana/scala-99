package alvaro

package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object P05 {
  def reverse(list: List[Int]): List[Int] = {
    @tailrec
    def rl(r: List[Int], l: List[Int]): List[Int] = {
      l match {
        case Nil => r
        case _ => rl(l.head :: r, l.tail)
      }
    }
    rl(Nil, list)
  }
}

class P05Spec extends WordSpecLike with Matchers {

  "reverse" should {
    import alvaro.P05._
    "return empty list when the list is empty" in {
      reverse(Nil) shouldBe Nil

    }
    "return (1) when the list is (1)" in {
      reverse(List(1)) shouldBe List(1)
    }
    "return (2,1) when the list is (1,2)" in {
      reverse(List(1, 2)) shouldBe List(2, 1)
    }
    "return (3,2,1) when the list is (1,2,3)" in {
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    }
  }

  "reverse with foldleft" should {
    def append(l: List[Int], e: Int) = e :: l

    "return empty list when the list is empty" in {
      Nil.foldLeft(List[Int]())(append _) shouldBe Nil
    }

    "return (1) when the list is (1)" in {
      List(1).foldLeft(List[Int]())(append _) shouldBe List(1)
    }
    "return (2,1) when the list is (1,2)" in {
      List(1, 2).foldLeft(List[Int]())(append _) shouldBe List(2, 1)
    }
    "return (3,2,1) when the list is (1,2,3)" in {
      List(1, 2, 3).foldLeft(List[Int]())(append _) shouldBe List(3, 2, 1)
    }
  }
}