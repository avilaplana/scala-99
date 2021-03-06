package alvaro

import org.scalatest.{WordSpecLike, Matchers, FlatSpec}

import scala.annotation.tailrec

object Finder {
  @tailrec
  def last(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case head :: Nil => Some(head)
      case head :: tail => last(tail)
    }
  }
}

class P01Spec extends WordSpecLike with Matchers {

  import Finder._

  "last" should {
    "return the last element in the list" in {
      val list = List(1, 1, 2, 3, 5, 8)
      last(list) shouldBe Some(8)
      last(Nil) shouldBe None
    }
  }
}
