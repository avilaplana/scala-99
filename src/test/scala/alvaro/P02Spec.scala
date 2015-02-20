package alvaro

import org.scalatest.{WordSpecLike, Matchers, FlatSpec}

import scala.annotation.tailrec

object P02SpecFinder {
  @tailrec
  def penultimate(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case one :: Nil => None
      case head :: last :: Nil => Some(head)
      case head :: tail => penultimate(tail)
    }
  }
}

class P02Spec extends WordSpecLike with Matchers {

  import P02SpecFinder._

  "penultimate" should {
    "return 5 when the list is 1, 1, 2, 3, 5, 8" in {
      val list = List(1, 1, 2, 3, 5, 8)
      penultimate(list) shouldBe Some(5)
    }

    "return 3 when the list is 1, 1, 2, 3, 5" in {
      val list = List(1, 1, 2, 3, 5)
      penultimate(list) shouldBe Some(3)
    }

    "return None when the list is 1" in {
      val list = List(1)
      penultimate(list) shouldBe None
    }

    "return None when the list is empty" in {
      val list = Nil
      penultimate(list) shouldBe None
    }
  }
}
