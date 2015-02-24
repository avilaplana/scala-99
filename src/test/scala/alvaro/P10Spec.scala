package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P10Spec extends WordSpecLike with Matchers {

  "enconde with foldleft" should {
    def encode(l: List[(Int, Char)], e: Char): List[(Int, Char)] =
      l match {
        case Nil => Nil :+(1, e)
        case i :+ s if (s._2 == e) => i :+ (s._1 + 1, e)
        case _ => l :+ (1, e)

      }
    "return List((1,'a')) when the list is List('a')" in {
      List('a').foldLeft(List[(Int, Char)]())(encode _) shouldBe List((1, 'a'))
    }
    "return List((2,'a')) when the list is List('a', 'a')" in {
      List('a', 'a').foldLeft(List[(Int, Char)]())(encode _) shouldBe List((2, 'a'))
    }
    "return List((1,'a'), (1,'b')) when the list is List('a', 'b')" in {
      List('a', 'b').foldLeft(List[(Int, Char)]())(encode _) shouldBe List((1, 'a'), (1, 'b'))
    }

    "return List((1,'a'), (1,'b'), (1,'a')) when the list is List('a', 'b', 'a')" in {
      List('a', 'b', 'a').foldLeft(List[(Int, Char)]())(encode _) shouldBe List((1, 'a'), (1, 'b'), (1, 'a'))
    }
    "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')) when the list is " +
      "List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {

      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      l.foldLeft(List[(Int, Char)]())(encode _) shouldBe List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
    }
  }
}
