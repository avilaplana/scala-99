package alvaro

import org.scalatest.{WordSpecLike, Matchers}

class P39Spec extends WordSpecLike with Matchers {

  import P31._
  def listPrimesinRange(r: Range):List[Int] = r.filter(isPrime(_)).toList

  "listPrimesinRange" should {
    "return List(7, 11, 13, 17, 19, 23, 29, 31) when the range is 7 to 31" in {
      listPrimesinRange(7 to 31) shouldBe List(7, 11, 13, 17, 19, 23, 29, 31)
    }
    "return List(151,157,163,167,173,179,181,191,193,197, 199) when the range is 150 to 200" in {
      listPrimesinRange(150 to 200) shouldBe List(151,157,163,167,173,179,181,191,193,197,199)
    }
  }
}
