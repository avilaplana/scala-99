package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P13Spec extends WordSpecLike with Matchers {

  def encodeDirect(l: List[(Int, Char)], c: Char) =
    l match {
      case init :+ (t: (Int, Char)) if (c == t._2) => init :+(t._1 + 1, c)
      case _ => l :+(1, c)
    }

  "encodeDirect" should {
    "return List() when the list is List()" in {
      List().foldLeft(List.empty[(Int, Char)])(encodeDirect _) shouldBe List()
    }
    "return List((1,'a')) when list is List('a')" in {
      List('a').foldLeft(List.empty[(Int, Char)])(encodeDirect _) shouldBe List((1, 'a'))
    }
    "return List((2,'a')) when list is List('a','a')" in {
      List('a', 'a').foldLeft(List.empty[(Int, Char)])(encodeDirect _) shouldBe List((2, 'a'))
    }
    "return List((2,'a'), (1,'b')) when list is List('a','a','b')" in {
      List('a', 'a', 'b').foldLeft(List.empty[(Int, Char)])(encodeDirect _) shouldBe List((2, 'a'), (1, 'b'))
    }

    "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')) when the list is " +
      "List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {

      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      l.foldLeft(List.empty[(Int, Char)])(encodeDirect _) shouldBe List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
    }
  }

}
