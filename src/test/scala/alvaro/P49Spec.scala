package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P49 {
  val n1 = List("0", "1")

  def grayCode(n: Int): List[String] = {
    if (n == 1)
      n1
    else {
      val gc = grayCode(n - 1)
      gc.map("0" + _) ::: gc.reverse.map("1" + _)
    }
  }
}

class P49Spec extends WordSpecLike with Matchers {

  import P49._


  "1" should {
    "be 0,1" in {
      grayCode(1) shouldBe List("0", "1")
    }
  }

  "2" should {
    "be 00, 01, 11, 10" in {
      grayCode(2) shouldBe List("00", "01", "11", "10")
    }
  }

  "3" should {
    "be 000, 001, 011, 010, 110, 111, 101, 100" in {
      grayCode(3) shouldBe List("000", "001", "011", "010", "110", "111", "101", "100")
    }
  }

}
