package haroon

import org.scalatest.{WordSpec, Matchers}

object P06 {

  def isPalindrome(list: List[Int]) = {
    list == list.reverse
  }
}

class P06Spec extends WordSpec with Matchers {

  "P06 Spec" should {

    "return true for empty list" in {
      P06.isPalindrome(List()) shouldBe true
    }

    "return true for a palindrome list" in {
      P06.isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    }

    "return false for a non-palindrome list" in {
      P06.isPalindrome(List(2, 2, 3, 2, 1)) shouldBe false
    }
  }
}