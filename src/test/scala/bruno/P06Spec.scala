package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P06 {
  def isPalindrome[A](lst: List[A]): Boolean = lst match {
    case x :: List() => true
    case x :: xs if x == xs.last => isPalindrome(xs.init)
    case _ => false
  }
}

class P06Spec extends WordSpecLike with Matchers {

  "isPalindrome" should {
    "find out whether a list is a palindrome" in {
      P06.isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
      P06.isPalindrome(List(1, 1, 2, 3, 5, 8)) should be (false)
    }
  }
}
