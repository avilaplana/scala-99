package alvaro.romanNumbers

import org.scalatest.{Matchers, WordSpecLike}


object Gregorian {
  val mapValues = Map(4 -> "IV", 9 -> "IX")

  def toRomanNumer(g: Int): String = {

    def lessThanTen: Int => String = {
      ltt =>
        val lessThanFour: Int => String = e => List.fill(e)("I").mkString
        val betweenFiveAndNine: Int => String = e => "V" + List.fill(e - 5)("I").mkString
        ltt match {
          case e if e < 4 => lessThanFour(e)
          case e if e >= 5 && e < 9 => betweenFiveAndNine(e)
          case e if e == 4 => mapValues(e)
          case e if e == 9 => mapValues(e)

        }
    }

    def betweenTenAndThirtyNine: Int => String = {
      btan =>
        val lessThanFourty: Int => String = e => List.fill(e)("X").mkString

        val f = btan.toString.toList.head.asDigit
        val s = btan.toString.toList.last.asDigit
        val tens = lessThanFourty(f)
        val units = lessThanTen(s)

        tens + units
    }


    def betweenFourtyAndFourtyNine: Int => String = {
      btan =>
        val s = btan.toString.toList.last.asDigit
        val units = lessThanTen(s)

        "XL" + units
    }

    def betweenFiftyAndEightyNine: Int => String = {
      btan =>
        val lessThanNinety: Int => String = e => "L"+ List.fill(e - 5)("X").mkString

        val f = btan.toString.toList.head.asDigit
        val s = btan.toString.toList.last.asDigit
        val tens = lessThanNinety(f)
        val units = lessThanTen(s)

        tens + units
    }

    g match {
      case e if e < 10 => lessThanTen(e)
      case e if e >= 10 && e <= 39 => betweenTenAndThirtyNine(e)
      case e if e >= 40 && e <= 49 => betweenFourtyAndFourtyNine(e)
      case e if e >= 50 && e <= 89 => betweenFiftyAndEightyNine(e)
      case _ => ???
    }
  }

  //  mapValues(g)
}

class RomanNumberSpec extends WordSpecLike with Matchers {


    "the roman number of 1" should {
      "be I" in {
        Gregorian.toRomanNumer(1) shouldBe "I"
      }
    }

    "the roman number of 2" should {
      "be II" in {
        Gregorian.toRomanNumer(2) shouldBe "II"
      }
    }

    "the roman number of 3" should {
      "be III" in {
        Gregorian.toRomanNumer(3) shouldBe "III"
      }
    }

    "the roman number of 4" should {
      "be IV" in {
        Gregorian.toRomanNumer(4) shouldBe "IV"
      }
    }

    "the roman number of 5" should {
      "be V" in {
        Gregorian.toRomanNumer(5) shouldBe "V"
      }
    }

    "the roman number of 6" should {
      "be VI" in {
        Gregorian.toRomanNumer(6) shouldBe "VI"
      }
    }

    "the roman number of 8" should {
      "be VIII" in {
        Gregorian.toRomanNumer(8) shouldBe "VIII"
      }
    }

  "the roman number of 10" should {
    "be X" in {
      Gregorian.toRomanNumer(10) shouldBe "X"
    }
  }

  "the roman number of 15" should {
    "be XV" in {
      Gregorian.toRomanNumer(15) shouldBe "XV"
    }
  }

  "the roman number of 39" should {
    "be XXXIX" in {
      Gregorian.toRomanNumer(39) shouldBe "XXXIX"
    }
  }


  "the roman number of 45" should {
    "be XLV" in {
      Gregorian.toRomanNumer(45) shouldBe "XLV"
    }
  }

  "the roman number of 65" should {
    "be LXV" in {
      Gregorian.toRomanNumer(65) shouldBe "LXV"
    }
  }

}
