package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Determine whether a given integer number is prime.
class P31Spec extends WordSpecLike with Matchers {

  def isPrime(p: Int): Boolean = Range(1, p + 1).filter(p % _ == 0) == Seq(1, p)


  "isPrime" should {
    "return false when the number is 1" in {
      isPrime(1) shouldBe false
    }
    "return true when the number is 2" in {
      isPrime(2) shouldBe true
    }
    "return true when the number is 3" in {
      isPrime(3) shouldBe true
    }
    "return false when the number is 4" in {
      isPrime(4) shouldBe false
    }
    "return true when the number is 7" in {
      isPrime(7) shouldBe true
    }
    "return true when the number is 17" in {
      isPrime(17) shouldBe true
    }
    "return true when the number is 37" in {
      isPrime(37) shouldBe true
    }
  }
}
