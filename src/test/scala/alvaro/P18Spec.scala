package alvaro

import org.scalatest.{Matchers, WordSpecLike}


/*
Given two indices, I and K, the slice is the list
 containing the elements from and including the
 Ith element up to but not including the Kth element
 of the original list. Start counting the elements with 0.
 */

class P18Spec extends WordSpecLike with Matchers {

  def slice(list: List[Char])(i: Int, k: Int) = list.slice(i, k)

  "slice" should {
    "return List('a') when the list is List('a','b') and the I is 0 and K is 1" in {
      slice(List('a', 'b'))(0, 1) shouldBe List('a')
    }
    "return List('a','b') when the list is List('a','b','c') and the I is 0 and K is 2" in {
      slice(List('a', 'b', 'c'))(0, 2) shouldBe List('a', 'b')
    }
    "return List('b') when the list is List('a','b','c') and the I is 1 and K is 2" in {
      slice(List('a', 'b', 'c'))(1, 2) shouldBe List('b')
    }
    "return List('d', 'e', 'f', 'g') when the list is List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') " +
      "and the I is 3 and K is 7" in {
      slice(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))(3, 7) shouldBe List('d', 'e', 'f', 'g')
    }
  }
}
