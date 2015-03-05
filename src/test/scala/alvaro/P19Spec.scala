package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object P19 {
  @tailrec
  def rotate(l: List[Char])(n: Int): List[Char] =
    n match {
      case 0 => l
      case n if n > 0 => rotate(l.tail :+ l.head)(n - 1)
      case _ => rotate(l.last :: l.init )(n + 1)
    }
}
class P19Spec extends WordSpecLike with Matchers {

  "rotate" should {
    import P19._
    "return List('a','b') when the list is List('a','b') and number of places is 0" in {
      rotate(List('a','b'))(0) shouldBe List('a','b')
    }

    "return List('b', 'a') when the list is List('a','b') and number of places is 1" in {
      rotate(List('a','b'))(1) shouldBe List('b', 'a')
    }

    "return List('a', 'b') when the list is List('a','b') and number of places is 2" in {
      rotate(List('a','b'))(2) shouldBe List('a', 'b')
    }

    "return List('c','a', 'b') when the list is List('a','b','c') and number of places is 2" in {
      rotate(List('a','b','c'))(2) shouldBe List('c','a', 'b')
    }

    "return List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c') when the list is " +
      "List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') and number of places is 3" in {
      rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))(3) shouldBe List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
    }

    "return List('b', 'a') when the list is List('a','b') and number of places is -1" in {
      rotate(List('a','b'))(-1) shouldBe List('b', 'a')
    }

    "return List('a', 'b') when the list is List('a','b') and number of places is -2" in {
      rotate(List('a','b'))(-2) shouldBe List('a', 'b')
    }

    "return List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i') when the list is List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') and number of places is -2" in {
      rotate(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))(-2) shouldBe List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
    }
  }
}
