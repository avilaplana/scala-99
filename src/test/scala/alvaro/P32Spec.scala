package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Determine the greatest common divisor of two positive integer numbers.
//Use Euclid's algorithm.
//scala> gcd(36, 63)
//res0: Int = 9

object P32 {

  import P31._

  def gcd(a: Int, b: Int): Int = {
    if (isPrime(a) && isPrime(b)) 1
    else {
      a % b match {
        case 0 => b
        case mod => gcd(b, mod)
      }
    }
  }
}

class P32Spec extends WordSpecLike with Matchers {

  import P32._

  "gcd" should {
    "return 9 when the numbers are 36 and 63" in {
      gcd(36, 63) shouldBe 9
    }
    "return 3 when the numbers are 45 and 21" in {
      gcd(45, 21) shouldBe 3
    }
    "return 2 when the numbers are 93164 and 5826" in {
      gcd(93164, 5826) shouldBe 2
    }
    "return 1 when the numbers are 89 and 55" in {
      gcd(89, 55) shouldBe 1
    }

    "return 1 when the numbers are 47 and 17" in {
      gcd(47, 17) shouldBe 1
    }
  }
}
