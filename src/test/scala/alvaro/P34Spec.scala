package alvaro

import org.scalatest.{Matchers, WordSpecLike}

/*
Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m)
that are coprime to m.
scala> 10.totient
res0: Int = 4
*/

object P34 {
  import P32._
  def totient: Int => Int = m => Range(1, m + 1).filter(gcd(_, m) == 1).size
}
class P34Spec extends WordSpecLike with Matchers {

  import P34._

  "totient" should {
    "return 4 when the number is 10" in {
      totient(10) shouldBe 4
    }
    "return 72 when the number is 234" in {
      totient(234) shouldBe 72
    }
    "return 616 when the number is 1234" in {
      totient(1234) shouldBe 616
    }
    "return 240 when the number is 287" in {
      totient(287) shouldBe 240
    }

  }
}
