package alvaro

import org.scalatest.{Matchers, WordSpecLike}

/*
Remove the Kth element from a list.
Return the list and the removed element
in a Tuple. Elements are numbered from 0.
*/

class P20Spec extends WordSpecLike with Matchers {

  def removeAt(l: List[Char])(i: Int): (List[Char], Char) = {
    if (!l.isDefinedAt(i)) throw new IllegalArgumentException(s"position:$i is greater than size:${l.size}")
    val (s1, s2) = l.splitAt(i)
    (s1 ::: s2.tail, l(i))
  }

  "removeAt" should {
    "return (List(),'a') when the list is List('a') and the element to remove is in 0" in {
      removeAt(List('a'))(0) shouldBe(List(), 'a')
    }
    "return (List('a'),'b') when the list is List('a','b') and the element to remove is in 1" in {
      removeAt(List('a','b'))(1) shouldBe(List('a'), 'b')
    }
    "return (List('a','b'),'c') when the list is List('a','b','c') and the element to remove is in 2" in {
      removeAt(List('a','b','c'))(2) shouldBe(List('a','b'), 'c')
    }

    "return (List('a', 'c', 'd'),'b') when the list is List('a', 'b', 'c', 'd') and the element to remove is in 1" in {
      removeAt(List('a', 'b', 'c', 'd'))(1) shouldBe(List('a', 'c', 'd'),'b')
    }
  }
}
