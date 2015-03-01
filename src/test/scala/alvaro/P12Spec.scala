package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P12Spec extends WordSpecLike with Matchers {

  import List._

  def decode(l: List[Any]): List[Char] =
    l flatMap {
      case c: Char => List(c)
      case t: (Int, Char) => fill(t._1)(t._2)
      case _ => ???
    }

  "decode" should {
    "return List() when the list is List()" in {
      decode(List()) shouldBe List()
    }
    "return List('a') when the list is List('a')" in {
      decode(List('a')) shouldBe List('a')
    }

    "return List('a','a') when the list is List((2, 'a'))" in {
      decode(List((2, 'a'))) shouldBe List('a', 'a')
    }

    "return List('a','a','b') when the list is List((2, 'a'),'b')" in {
      decode(List((2, 'a'),'b')) shouldBe List('a','a','b')
    }
    "return List('a','a','b','b') when the list is List((2, 'a'),(2, 'b'))" in {
      decode(List((2, 'a'),(2, 'b'))) shouldBe List('a','a','b','b')
    }
    "return List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" +
      " when the list is List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))" in {

      val l = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
      decode(l) shouldBe List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    }

  }
}
