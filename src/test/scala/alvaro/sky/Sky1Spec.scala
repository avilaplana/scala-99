package alvaro.sky


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


  case class Time(hour: Int, min: Int, sec: Int) {
    def seconds: Int = hour * 60 * 60 + min * 60 + sec

    def calc: Int = {
      if (min <= 4) (min * 60 + sec) * 3
      else {
        val total = (hour * 60 + min) * 150
        if (sec > 0) total + 150 else total
      }
    }
  }

  case class Number(n: String) extends AnyVal

  case class TelephoneCall(time: Time, number: Number)

  object TelephoneCall {
    def apply(call: String): TelephoneCall = {
      val c = call.split(",")
      val t = c(0).split(":")

      TelephoneCall(
        time = Time(t(0).toInt, t(1).toInt, t(2).toInt),
        number = Number(c(1))
      )
    }
  }

  def cost(calls: Seq[String]): Seq[Int] = {
    val allCalls: Seq[TelephoneCall] = calls.map(TelephoneCall(_))
    val totByNumber: List[(Number, Int)] = allCalls.groupBy(_.number).map(a =>(a._1, a._2.map(_.time.seconds).sum)).toList
    val freeNumber: Number = totByNumber.sortWith((n1,n2) => n1._2 > n2._2).head._1
    allCalls.map(c => if (c.number == freeNumber) 0 else c.time.calc)
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
