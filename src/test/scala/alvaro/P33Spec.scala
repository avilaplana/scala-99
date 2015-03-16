package alvaro

import org.scalatest.{Matchers, WordSpecLike}

/*
Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
scala> 35.isCoprimeTo(64)
res0: Boolean = true
*/
class P33Spec extends WordSpecLike with Matchers {

  "isCoprime" should {
    import P32._
    "return true if the numbers are 35, 64" in {
      gcd(35, 64) == 1  shouldBe true
    }
    "return true if the numbers are 47, 5" in {
      gcd(47, 5) == 1 shouldBe true
    }
    "return true if the numbers are 23452, 23457" in {
      gcd(23452, 23457) == 1 shouldBe true
    }
    "return false if the numbers are 231948, 1232" in {
      gcd(231948, 1232) == 1 shouldBe false
    }

  }

}
