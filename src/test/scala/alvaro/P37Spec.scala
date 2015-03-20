package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P37 {

  import P36._
  import scala.math._

  def totient(m: Int): Double =
    m.primeFactors.toMultilicityMap.map {
      b => (b._1.toDouble - 1) * pow(b._1.toDouble, b._2 - 1)
    }.foldLeft(1D) { (p, e) => p * e}
}

class P37Spec extends WordSpecLike with Matchers {

  "totient" should {
    "be the same for number 900" in {
      P37.totient(900) shouldBe P34.totient(900)
    }
    "be the same for number 45" in {
      P37.totient(45) shouldBe P34.totient(45)
    }
    "be the same for number 1456" in {
      P37.totient(1456) shouldBe P34.totient(1456)
    }
    "nt be the same for number 1456 and number 200" in {
      P37.totient(200) shouldNot be(P34.totient(1456))
    }
  }
}
