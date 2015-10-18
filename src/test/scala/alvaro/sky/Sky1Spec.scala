package alvaro.sky


import alvaro.sky.CalculatorService.TelephoneCall
import org.scalatest.{Matchers, WordSpecLike}

import scala.util.matching.Regex

object CalculatorService {

  case class TelephoneCall(hour: Int, min: Int, sec: Int, num: String) {
    val durationInSec: Int = hour * 3600 + min * 60 + sec
    lazy val numDigit = num.filterNot(_ == '-').toInt
  }

  object TelephoneCall {
    val regEx = new Regex(
      "^([0-9]{2}):([0-5][0-9]):([0-5][0-9]),([1-9][0-9]{2}-[0-9]{3}-[0-9]{3})$",
      "hour", "min", "sec", "num")

    def apply(call: String): TelephoneCall = {
      regEx findFirstMatchIn call match {
        case Some(m) =>
          TelephoneCall(
            hour = m.group("hour").toInt,
            min = m.group("min").toInt,
            sec = m.group("sec").toInt,
            num = m.group("num")
          )
        case None => throw new IllegalArgumentException(s"The format of $call is not correct")
      }
    }

    def charge(sec: Int): Int = {
      val lessThanFiveMinutes: Int => Boolean = _ <= 299
      val lastMinStarted: Int => Boolean = _ % 60 != 0

      sec match {
        case s if lessThanFiveMinutes(s) => s * 3
        case s if lastMinStarted(s) => (s / 60) * 150 + 150
        case s => (s / 60) * 150
      }
    }
  }

  def solution(c: String): Int = {

    val logs = c.lines.toSeq
    if (logs.isEmpty || logs.size > 100) throw new IllegalArgumentException(s"Number of calls must between 1..100")

    val calls = logs.map(TelephoneCall(_))
    val durationByNumber = calls.groupBy(_.numDigit).map(a => (a._1, a._2.map(_.durationInSec).sum)).toSeq
    val sortedByDurationDesc = durationByNumber.sortWith((n1, n2) => n1._2 > n2._2)
    val candidates = sortedByDurationDesc.takeWhile(_._2 == sortedByDurationDesc.head._2)
    val freeNumber = candidates.sortWith((n1, n2) => n1._1 < n2._1).head._1
    calls.map {
      case c if (c.numDigit == freeNumber) => 0
      case c => TelephoneCall.charge(c.durationInSec)
    }.sum
  }
}

class Sky1Spec extends WordSpecLike with Matchers {

  Seq(
    "   00:59:00,111-111-111    ",
    "00:60:00,111-111-111",
    "00:00:60,111-111-111",
    "00:00:10,011-111-111",
    "00:00:10,11-111-111",
    "00:00:10,111-11-111",
    "00:00:10,111-111-11",
    "00:0:10,111-111-111",
    "00:00:1,111-111-111",
    "0:00:10,011-111-111",
    "00:00:10",
    "00:00:10,",
    ",011-111-111",
    "00:00,111-111-111",
    "00,111-111-111",
    "00:00:10,111-",
    "00:00:10,111-111"
  ).foreach(bf => s"$bf" should {
    "raise an IllegalArgumentException" in {
      val thrown = the[IllegalArgumentException] thrownBy TelephoneCall(bf)
      thrown.getMessage shouldBe s"The format of $bf is not correct"
    }
  })

  "telephone call 00:00:45,112-112-111" should {
    "be transformend into TelephoneCall object" in {
      val tc = TelephoneCall("00:00:45,112-112-111")
      tc.durationInSec shouldBe 45
      tc.numDigit shouldBe 112112111
    }
  }

  "telephone call 00:01:00,111-111-111" should {
    "be transformend into TelephoneCall object" in {
      val tc = TelephoneCall("00:01:00,111-111-111")
      tc.durationInSec shouldBe 60
      tc.numDigit shouldBe 111111111
    }
  }

  "telephone call 99:59:59,111-111-111" should {
    "be transformend into TelephoneCall object" in {
      val tc = TelephoneCall("99:59:59,111-111-111")
      tc.durationInSec shouldBe (99 * 3600 + 59 * 60 + 59)
      tc.numDigit shouldBe 111111111
    }
  }


  "00:01:00,111-111-111" should {
    "cost 180p" in {
      val tc = TelephoneCall("00:01:00,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 180
    }
  }

  "00:04:59,111-111-111" should {
    "cost 897p" in {
      val tc = TelephoneCall("00:04:59,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 897
    }
  }

  "00:05:00,111-111-111" should {
    "cost 750p" in {
      val tc = TelephoneCall("00:05:00,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 750

    }
  }

  "00:05:01,111-111-111" should {
    "cost 900p" in {
      val tc = TelephoneCall("00:05:01,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 900

    }
  }

  "99:59:00,111-111-111" should {
    "cost 899850p" in {
      val tc = TelephoneCall("99:59:00,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 899850
    }
  }

  "99:59:01,111-111-111" should {
    "cost 900000p" in {
      val tc = TelephoneCall("99:59:01,111-111-111")
      TelephoneCall.charge(tc.durationInSec) shouldBe 900000
    }
  }

  val twoCalls = """00:01:00,111-111-111
                   |00:05:01,222-222-222""".stripMargin

  "2 calls" should {
    "cost 180 and 0" in {
      CalculatorService.solution(twoCalls) shouldBe 180
    }
  }

  val threeCalls = """00:01:00,111-111-111
                     |00:05:01,222-222-222
                     |99:59:00,222-222-333""".stripMargin

  "3 calls" should {
    "cost 180, 900 and 0" in {
      CalculatorService.solution(threeCalls) shouldBe 1080
    }
  }
  //    "0 calls" should {
  //      "raise an exception" in {
  //        val thrown = the [IllegalArgumentException] thrownBy TelephoneCall("")
  //        thrown.getMessage shouldBe "Number of calls must between 1..100000"
  //      }
  //    }


  "3 calls with 2 wth the same duration" should {
    "cost 180, 900 and 0" in {

      val threeCallsWith2SameDuration = """00:01:00,111-111-111
                                          |00:05:01,222-222-222
                                          |00:05:01,222-222-333""".stripMargin
      CalculatorService.solution(threeCallsWith2SameDuration) shouldBe 1080
    }
  }

  "4 calls with 2 wth the same duration" should {
    "cost 180, 900 and 0" in {
      val fourCallsWith2SameDuration = """00:01:00,111-111-111
                                         |00:05:01,222-222-222
                                         |00:01:00,222-222-222
                                         |00:05:01,222-222-333""".stripMargin

      CalculatorService.solution(fourCallsWith2SameDuration) shouldBe 1080
    }
  }

  "11 calls with 3 wth the same duration" should {
    "cost 180, 900 and 0" in {
      val elevenCallsWith2SameDuration = """00:01:00,111-111-111
                                           |00:05:01,222-222-221
                                           |23:05:01,222-222-222
                                           |23:05:02,222-222-222
                                           |00:05:01,222-222-223
                                           |23:05:01,222-222-224
                                           |23:05:02,222-222-224
                                           |00:05:01,222-222-333
                                           |23:05:01,222-222-111
                                           |23:05:02,222-222-111
                                           |00:05:01,222-222-333""".stripMargin

      CalculatorService.solution(elevenCallsWith2SameDuration) shouldBe 835380
    }
  }



  "00:01:07,400-234-090 00:05:01,701-080-080 00:05:00,400-234-090" should {
    "cost 900" in {
      val example = """00:01:07,400-234-090
                      |00:05:01,701-080-080
                      |00:05:00,400-234-090""".stripMargin
      CalculatorService.solution(example) shouldBe 900

    }
  }

}
