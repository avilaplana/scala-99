package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P11Spec extends WordSpecLike with Matchers {

  "encodeModified" should {
    def encodeModified(l: List[Any], c: Char): List[Any] = {
      l match {
        case Nil => List(c)
        case init :+ (t: (Int, Char)) if (c == t._2) => init :+ (t._1 + 1, c)
        case init :+ d if (c == d) => init :+ (2, d)
        case _ => l :+ c
      }
    }
    "return List() when the list is empty List()" in {
      List().foldLeft(List[Any]())(encodeModified _) shouldBe List()
    }
    "return List('a') when the list is List('a')" in {
      List('a').foldLeft(List[Any]())(encodeModified _) shouldBe List('a')
    }
    "return List((2,'a')) when the list is List('a','a')" in {
      List('a', 'a').foldLeft(List[Any]())(encodeModified _) shouldBe List((2, 'a'))
    }
    "return List((2,'a'),'b') when the list is List('a','a','b')" in {
      List('a', 'a', 'b').foldLeft(List[Any]())(encodeModified _) shouldBe List((2, 'a'), 'b')
    }
    "return List((2,'a'),(2,'b')) when the list is List('a','a','b','b')" in {
      List('a','a','b','b').foldLeft(List[Any]())(encodeModified _) shouldBe List((2,'a'),(2,'b'))
    }
    "return List('a','b','a') when the list is List('a','b','a')" in {
      List('a','b','a').foldLeft(List[Any]())(encodeModified _) shouldBe List('a','b','a')
    }
    "return List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e')) when the list is " +
      "List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {

      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      l.foldLeft(List[Any]())(encodeModified _) shouldBe List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))
    }

  }
}
