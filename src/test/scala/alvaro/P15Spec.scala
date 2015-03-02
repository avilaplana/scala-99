package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P15Spec extends WordSpecLike with Matchers {

  def duplicateN(l: List[Char])(n: Int): List[Char] = l.flatMap(c => List.fill(n)(c))

  "duplicateN" should {
    "return List() when the list is List()" in {
      duplicateN(List())(100) shouldBe List()
    }
    "return List('a','a') when the list is List('a') and N is 1" in {
      duplicateN(List('a'))(1) shouldBe List('a')
    }

    "return List('a','a') when the list is List('a') and N is 2" in {
      duplicateN(List('a'))(2) shouldBe List('a','a')
    }
    "return List('a','b') when the list is List('a','b') and N is 1" in {
      duplicateN(List('a','b'))(1) shouldBe List('a','b')
    }
    "return List('a','a','b','b') when the list is List('a','b') and N is 2" in {
      duplicateN(List('a','b'))(2) shouldBe List('a','a','b','b')
    }
    "return List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd) when the list " +
      "is List('a', 'b', 'c', 'c', 'd') and N is 3" in {
      duplicateN(List('a', 'b', 'c', 'c', 'd'))(3) shouldBe List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')
    }

  }

}
