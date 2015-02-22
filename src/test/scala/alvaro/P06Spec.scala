package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P06 {
  def isPalindrome(list: List[Int]): Boolean = {
    def fp(l: List[Int]): Boolean =
      l match {
        case Nil => true
        case head :: Nil => true
        case l if (l.head == l.last) => fp(l.tail.init)
        case l if (l.head != l.last) => false
      }
    fp(list)
  }
}

class P06Spec extends WordSpecLike with Matchers {

  "isPalindrome" should {
    import P06._
    "return true when the list is empty" in {
      isPalindrome(Nil) shouldBe true
    }
    "return true when the list is (1)" in {
      isPalindrome(List(1)) shouldBe true
    }
    "return true when the list is (1,1)" in {
      isPalindrome(List(1, 1)) shouldBe true
    }
    "return true wen the list is (1, 2, 1) " in {
      isPalindrome(List(1, 2, 1)) shouldBe true
    }
    "return true wen the list is (1, 2, 2, 1) " in {
      isPalindrome(List(1, 2, 2, 1)) shouldBe true
    }
    "return true when the list is (1, 2, 3, 2, 1) " in {
      isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    }
    "return false when the list is (1,2,3)" in {
      isPalindrome(List(1, 2, 3)) shouldBe false
    }


  }

}
