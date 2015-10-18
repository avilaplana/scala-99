package alvaro.sky

import scala.util.matching.Regex

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

  def findFreeNumber(calls: Seq[TelephoneCall]): Int = {
    val durationByNumber = calls.groupBy(_.numDigit).map(a => (a._1, a._2.map(_.durationInSec).sum)).toSeq
    val sortedByDurationDesc = durationByNumber.sortWith((n1, n2) => n1._2 > n2._2)
    val candidates = sortedByDurationDesc.takeWhile(_._2 == sortedByDurationDesc.head._2)
    candidates.sortWith((n1, n2) => n1._1 < n2._1).head._1
  }
}

object CalculatorService {

  import TelephoneCall._

  def solution(c: String): Int = {
    val logs = c.lines.toSeq
    if (logs.isEmpty || logs.size > 100) throw new IllegalArgumentException(s"Number of calls must between 1..100")

    val calls = logs.map(TelephoneCall(_))
    val freeNumber = findFreeNumber(calls)
    calls.map {
      case c if (c.numDigit == freeNumber) => 0
      case c => charge(c.durationInSec)
    }.sum
  }
}