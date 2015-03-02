package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P16Spec extends WordSpecLike with Matchers {

  "drop" should {
    def drop(nth: Int, l: List[Char]) : List[Char] = {
      if (nth > l.size) throw new IllegalArgumentException(s"nth:$nth greater than size:${l.size}")
      val (init,tail) = l.splitAt(nth - 1)
      init ::: tail.tail
    }
    "return a List() when the List is List('a') and the element to remove 1st element" in {
      drop(1,List('a')) shouldBe List()
    }
    "return a List('b') when the List is List('a','b') and the element to remove 1st element" in {
      drop(1,List('a','b')) shouldBe List('b')
    }
    "return a List('a','c') when the List is List('a','b','c') and the element to remove 2nd element" in {
      drop(2,List('a','b','c')) shouldBe List('a','c')
    }

    "return a List('a','b') when the List is List('a','b','c') and the element to remove 3nd element" in {
      drop(3,List('a','b','c')) shouldBe List('a','b')
    }

    "return a List('a', 'b', 'd', 'e', 'f','g', 'h', 'j', 'k') when the List is " +
      "List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') and the element to remove 3nd element" in {

      drop(3,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) shouldBe List('a', 'b', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    }

    "return IllegalArgumentException when the position is higher than the size of the list" in {
      val e = intercept[IllegalArgumentException]{
        drop(3, List())
      }
      e.getMessage shouldBe "nth:3 greater than size:0"
    }

  }

}