package alvaro.codility.sky


import org.scalatest.{Matchers, WordSpecLike}

//00:01:00,111-111-111
//00:05:01,222-222-222
//00:05:00,111-111-111
//
//
//Each call is:
//- 3p per second if the call duration is less than 5 minutes; 00:01:00 => 180p
//- 150p per minute (for each started minute) if the call is more or equal to 5 minutes; 00:05:01 => 900
//- calls made to the number with the longest total duration are for free (so in the example we get 900p
// as calls to 111-111-111 are for free)
//
//Max. number of records = 100

object CalculatorService {


  val SecondsPerHour = 3600
  val SecondsPerMinute = 60

  case class Time(hour: Int, min: Int, sec: Int) {

    if (min > 60 || sec > 60) throw new IllegalArgumentException("Minutes and seconds must be less than 60")

    val totalSecs: Int = hour * SecondsPerHour + min * SecondsPerMinute + sec

    def cost: Int = {
      val lessThanFourMinutes: Int => Boolean = _ <= 240
      val lastMinStarted: Int => Boolean = _ % 60 != 0

      totalSecs match {
        case s if lessThanFourMinutes(s) => s * 3
        case s if lastMinStarted(s) => (s / 60) * 150 + 150
        case s => (s / 60) * 150
      }
    }
  }


  case class Number(n: String) extends AnyVal

  case class TelephoneCall(time: Time, number: Number)

  object TelephoneCall {
    val regEx = "([0-9]{2}):([0-5]?[0-9]):([0-5]?[0-9]),(.*)".r

    def apply(call: String): TelephoneCall = {
      regEx findFirstMatchIn call match {
        case Some(m) =>
          TelephoneCall(
            time = Time(
              hour = m.group(1).toInt,
              min = m.group(2).toInt,
              sec = m.group(3).toInt),
            number = Number(m.group(4))
          )
        case None => throw new IllegalArgumentException(s"The format of $call is not correct")
      }
    }
  }

  def cost(c: Seq[String]): Seq[Int] = {
    val calls: Seq[TelephoneCall] = c.map(TelephoneCall(_))
    val secondsByNum: List[(Number, Int)] = calls.groupBy(_.number).map(a => (a._1, a._2.map(_.time.totalSecs).sum)).toList
    val freeNumber: Number = secondsByNum.sortWith((n1, n2) => n1._2 > n2._2).head._1
    calls.map {
      case c if (c.number == freeNumber) => 0
      case c => c.time.cost
    }
  }
}

class Sky1Spec extends WordSpecLike with Matchers {

  "list of calls" should {
    "return the cost of all of them" in {
      val calls = Seq(
        "00:01:00,111-111-111",
        "00:05:01,222-222-222",
        "00:05:00,111-111-111",
        "01:05:00,111-111-111",
        "03:05:00,111-111-000")
      println(CalculatorService.cost(calls))
    }
  }
}
