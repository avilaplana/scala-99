package alvaro

import org.scalatest.{Matchers, WordSpecLike}

class P38Spec extends WordSpecLike with Matchers {
  "totient" should {
    "be the same for number 10090" in {
      P37.totient(10090) shouldBe P34.totient(10090)
    }
  }
}
