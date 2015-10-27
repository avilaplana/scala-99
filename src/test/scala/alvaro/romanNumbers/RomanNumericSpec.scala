package alvaro.romanNumbers

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object RomanNumeric {

  val chart = Map("I" -> 1, "IV" -> 4, "V" -> 5, "IX" -> 9, "X" -> 10, "XL" -> 40)

  def toArabic(rn: String): Int = {
    @tailrec
    def convert(it: Seq[Char], acc: Int): Int = {
      it match {
        case Nil => acc
        case f +: s +: t if chart.get(s"$f$s").isDefined => convert(t, acc + chart(s"$f$s"))
        case f +: t => convert(t, acc + chart(s"$f"))
      }
    }
    convert(rn.toList, 0)
  }
}

class RomanNumericSpec extends WordSpecLike with Matchers {

  "I" should {
    "be converted to 1" in {
      RomanNumeric.toArabic("I") shouldBe 1
    }
  }

  "II" should {
    "be converted to 2" in {
      RomanNumeric.toArabic("II") shouldBe 2
    }
  }

  "III" should {
    "be converted to 3" in {
      RomanNumeric.toArabic("III") shouldBe 3
    }
  }

  "IV" should {
    "be converted to 4" in {
      RomanNumeric.toArabic("IV") shouldBe 4
    }
  }

  "V" should {
    "be converted to 5" in {
      RomanNumeric.toArabic("V") shouldBe 5
    }
  }

  "VI" should {
    "be converted to 6" in {
      RomanNumeric.toArabic("VI") shouldBe 6
    }
  }

  "IX" should {
    "be converted to 9" in {
      RomanNumeric.toArabic("IX") shouldBe 9
    }
  }

  "XIV" should {
    "be converted to 14" in {
      RomanNumeric.toArabic("XIV") shouldBe 14
    }
  }

  "XXIII" should {
    "be converted to 23" in {
      RomanNumeric.toArabic("XXIII") shouldBe 23
    }
  }

  "XLVII" should {
    "be converted to 47" in {
      RomanNumeric.toArabic("XLVII") shouldBe 47
    }
  }

}
