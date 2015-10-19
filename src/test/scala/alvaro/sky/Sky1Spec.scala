package alvaro.sky

import org.scalatest.{Matchers, WordSpecLike}

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

  "00:01:07,400-234-090 00:05:01,701-080-080 00:05:00,400-234-090" should {
    "be 400234090" in {
      val example = Seq(TelephoneCall("00:01:07,400-234-090"),
        TelephoneCall("00:05:01,701-080-080"),
        TelephoneCall("00:05:00,400-234-090"))
      TelephoneCall.findFreeNumber(example) shouldBe 400234090
    }
  }

  "00:01:07,400-234-090 00:05:01,701-080-080 00:05:00,400-234-090" should {
    "be 701080080" in {
      val example = Seq(TelephoneCall("00:06:07,400-234-090"),
        TelephoneCall("01:05:01,701-080-080"),
        TelephoneCall("00:05:00,400-234-090"))
      TelephoneCall.findFreeNumber(example) shouldBe 701080080
    }
  }

  "00:01:07,100-000-000 01:07:01,100-000-001 00:01:07,100-000-000 01:07:01,100-000-001" should {
    "be 100000000" in {
      val example = Seq(
        TelephoneCall("00:01:08,100-000-000"),
        TelephoneCall("00:01:07,100-000-001"),
        TelephoneCall("01:07:00,100-000-000"),
        TelephoneCall("01:07:01,100-000-001"))
      TelephoneCall.findFreeNumber(example) shouldBe 100000000
    }
  }

  "00:01:07,100-000-000 01:07:01,100-000-001 00:01:07,100-000-000 01:07:01,100-000-001 02:07:01,100-000-002" should {
    "be 100000000" in {
      val example = Seq(
        TelephoneCall("00:01:08,100-000-000"),
        TelephoneCall("00:01:07,100-000-001"),
        TelephoneCall("01:07:00,100-000-000"),
        TelephoneCall("02:07:01,100-000-002"),
        TelephoneCall("01:07:01,100-000-001"))
      TelephoneCall.findFreeNumber(example) shouldBe 100000002
    }
  }


}
