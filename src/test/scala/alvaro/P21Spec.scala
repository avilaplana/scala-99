package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Insert an element at a given position into a list.
class P21Spec extends WordSpecLike with Matchers {

  "insertAt" should {
    def insertAt(c: Char, pos: Int, list: List[Char]) = {
      list match {
        case l if !l.isEmpty && pos + 1 > l.size => throw new IllegalArgumentException(s"position:$pos is greater than size:${l.size}")
        case _ =>
          val (i, t) = list.splitAt(pos)
          (i :+ c) ::: t
          //(list.take(pos) :+ c) ::: list.drop(pos)
      }
    }

    "return List('a') when the list is List() and the position is 0 and character is 'a'" in {
      insertAt('a', 2, List()) shouldBe List('a')
    }
    "return List('a','b') when the list is List('b') and the position is 0 and character is 'a'" in {
      insertAt('a', 0, List('b')) shouldBe List('a', 'b')
    }
    "return List('a', 'e', 'b, 'c, 'd) when the list is List('a', 'b', 'c', 'd') and the position is 1 and character is 'e'" in {
      insertAt('e', 1, List('a', 'b', 'c', 'd')) shouldBe List('a', 'e', 'b', 'c', 'd')
    }
  }
}
