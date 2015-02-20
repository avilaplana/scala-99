package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object P03 {
  @tailrec
  def nth(position: Int, list: List[Int]): Option[Int] =
    position match {
      case 0 => Some(list.head)
      case p if !list.isEmpty => nth(p - 1, list.tail)
      case _ => None
    }
}
class P03Spec extends WordSpecLike with Matchers {

  "nth" should {
    import P03._
    val list = List(5,4,3,2,1)

    "return 5 of a list 5,4,3,2,1 when Kth is 0" in {
      nth(0, list) shouldBe Some(5)
    }

    "return 4 of a list 5,4,3,2,1 when Kth is 1" in {
      nth(1, list) shouldBe Some(4)
    }
    "return 3 of a list 5,4,3,2,1 when Kth is 2" in {
      nth(2, list) shouldBe Some(3)
    }
    "the None when there is no element in Kth 10" in {
      nth(10, list) shouldBe None
    }
  }

}
