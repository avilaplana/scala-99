package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P14Spec extends WordSpecLike with Matchers {

  import List._

  def duplicate(l: List[Char]): List[Char] = l flatMap (c => fill(2)(c))

  "duplicate" should {
    "List() when the list is List()" in {
      duplicate(List()) shouldBe List()
    }
    "List('a', 'a') when the list is List('a')" in {
      duplicate(List('a')) shouldBe List('a', 'a')
    }
    "List('a', 'a', 'a', 'a') when the list is List('a','a')"in {
      duplicate(List('a', 'a')) shouldBe List('a', 'a', 'a', 'a')
    }
    "List('a', 'a', 'b', 'b', 'c', 'c') when the list is List('a','b', 'c')" in {
      duplicate(List('a','b', 'c')) shouldBe List('a', 'a', 'b', 'b', 'c', 'c')
    }
    "List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd') when the list is List('a', 'b', 'c', 'c', 'd')" in {
      val  l = List('a', 'b', 'c', 'c', 'd')
      duplicate(l) shouldBe List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd')
    }
  }
}
