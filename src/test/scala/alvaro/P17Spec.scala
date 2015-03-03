package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P17Spec extends WordSpecLike with Matchers {


  "split" should {
    def split(nth: Int, l: List[Char]): (List[Char], List[Char]) =
      (nth, l.size) match {
        case (_,0) => throw new IllegalArgumentException(s"nth:$nth greater than size:${l.size}")
        case (n,s) if (n > s) => throw new IllegalArgumentException(s"nth:$nth greater than size:${l.size}")
        case _ => l.splitAt(nth)
      }

    "return tuple List('a') List() when the list is List('a') and the position to split by is 1" in {
      split(1, List('a')) shouldBe(List('a'), List())
    }
    "return tuple List('a') List('b') when the list is List('a','b') and the position to split by is 1" in {
      split(1, List('a', 'b')) shouldBe(List('a'), List('b'))
    }
    "return tuple List('a','b') List('c') when the list is List('a','b','c') and the position to split by is 2" in {
      split(2, List('a', 'b', 'c')) shouldBe(List('a', 'b'), List('c'))
    }
    "return tuple List('a, 'b, 'c),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k') when the list is " +
      "List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') and the position to split by is 3" in {

      split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) shouldBe
        (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
    }

    "return IllegalArgumentException when the position is higher than the size of the list" in {
      val e = intercept[IllegalArgumentException] {
        split(3, List())
      }
      e.getMessage shouldBe "nth:3 greater than size:0"
    }

    "return IllegalArgumentException when the list to split is empty" in {
      val e = intercept[IllegalArgumentException] {
        split(0, List())
      }
      e.getMessage shouldBe "nth:0 greater than size:0"
    }

  }
}
