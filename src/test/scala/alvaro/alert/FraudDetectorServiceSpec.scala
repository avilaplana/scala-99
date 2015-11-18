package alvaro.alert

import java.util.Date

import org.scalatest.mock.MockitoSugar
import org.scalatest.{Matchers, WordSpecLike}
import org.mockito.Mockito._


class FraudDetectorServiceSpec extends WordSpecLike with Matchers with MockitoSugar {

  "one transaction of 10001" should {
    "submit an alert with this message 'Amount exceeded threshold'" in {

      val transUnderTest = Transaction("0123456789", new Date(), BigDecimal(10001))
      val alertUnderTest = Alert(transUnderTest, "Amount exceeded threshold")
      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      detector.process(transUnderTest)
      verify(alService).alert(alertUnderTest)
    }
  }

  "2 transaction of 10001 in 5 minutes" should {
    "submit an alert with this message 'Amount exceeded threshold'" in {

      val now = new Date().getTime
      val twoMinutesLater = now + 120000

      println(new Date(now))
      println(new Date(twoMinutesLater))

      val ftUnderTest = Transaction("0123456789", new Date(now), BigDecimal(901))
      val stUnderTest = Transaction("0123456789", new Date(twoMinutesLater), BigDecimal(9100))
      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      val alertUnderTest = Alert(stUnderTest, "Amount exceeded threshold")

      detector.process(ftUnderTest)
      verifyZeroInteractions(alService)

      detector.process(stUnderTest)
      verify(alService).alert(alertUnderTest)
    }
  }

  "2 transaction of 10001 in 31 minutes" should {
    "not submit an alert with this message 'Amount exceeded threshold'" in {

      val now = new Date().getTime
      val twoMinutesLater = now + 31 * 60 * 1000

      println(new Date(now))
      println(new Date(twoMinutesLater))

      val ftUnderTest = Transaction("0123456789", new Date(now), BigDecimal(901))
      val stUnderTest = Transaction("0123456789", new Date(twoMinutesLater), BigDecimal(9100))
      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      val alertUnderTest = Alert(stUnderTest, "Amount exceeded threshold")

      detector.process(ftUnderTest)
      verifyZeroInteractions(alService)

      detector.process(stUnderTest)
      verifyZeroInteractions(alService)
    }
  }


  "2 transaction of 10000 in 5 minutes" should {
    "submit an alert with this message 'Amount exceeded threshold'" in {

      val now = new Date().getTime
      val twoMinutesLater = now + 120000

      println(new Date(now))
      println(new Date(twoMinutesLater))

      val ftUnderTest = Transaction("0123456789", new Date(now), BigDecimal(900))
      val stUnderTest = Transaction("0123456789", new Date(twoMinutesLater), BigDecimal(9100))
      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      val alertUnderTest = Alert(stUnderTest, "Amount exceeded threshold")

      detector.process(ftUnderTest)
      verifyZeroInteractions(alService)

      detector.process(stUnderTest)
      verifyZeroInteractions(alService)
    }
  }


  "3 transaction of 10000 in 30 seconds" should {
    "submit an alert with this message 'Number of transactions exceeded threshold.'" in {

      val now = new Date().getTime
      val tenSeconsLater = now + 1000
      val twentySeconsLater = now + 2000

      println(new Date(now))
      println(new Date(tenSeconsLater))
      println(new Date(twentySeconsLater))

      val ftUnderTest = Transaction("0123456789", new Date(now), BigDecimal(900))
      val stUnderTest = Transaction("0123456789", new Date(tenSeconsLater), BigDecimal(1))
      val ttUnderTest = Transaction("0123456789", new Date(twentySeconsLater), BigDecimal(2))

      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      val alertUnderTest = Alert(ttUnderTest, "Number of transactions exceeded threshold")

      detector.process(ftUnderTest)
      verifyZeroInteractions(alService)

      detector.process(stUnderTest)
      verifyZeroInteractions(alService)

      detector.process(ttUnderTest)
      verify(alService).alert(alertUnderTest)
    }
  }

  "3 transaction of 10000 in 31 seconds" should {
    "not submit an alert with this message 'Number of transactions exceeded threshold.'" in {

      val now = new Date().getTime
      val tenSeconsLater = now + 1000
      val twentySeconsLater = now + 30001

      println(new Date(now))
      println(new Date(tenSeconsLater))
      println(new Date(twentySeconsLater))

      val ftUnderTest = Transaction("0123456789", new Date(now), BigDecimal(900))
      val stUnderTest = Transaction("0123456789", new Date(tenSeconsLater), BigDecimal(1))
      val ttUnderTest = Transaction("0123456789", new Date(twentySeconsLater), BigDecimal(2))

      val alService: FraudAlertService = mock[FraudAlertService]
      val detector: FraudDetectorService = new FraudDetectorService {
        override val alertService: FraudAlertService = alService
      }

      val alertUnderTest = Alert(ttUnderTest, "Number of transactions exceeded threshold")

      detector.process(ftUnderTest)
      verifyZeroInteractions(alService)

      detector.process(stUnderTest)
      verifyZeroInteractions(alService)

      detector.process(ttUnderTest)
      verifyZeroInteractions(alService)
    }
  }

}
