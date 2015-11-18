package alvaro.alert

import java.util.Date

import org.scalatest.{Matchers, WordSpecLike}

class FraudDetectorServiceIntSpec extends WordSpecLike with Matchers {

  "transctions" should {
    "raise alerts" in {
//      Timestamp    CC#                Amount
//      10:00:01.000 4111111111111111   150.00
//      10:00:05.000 4111111111111111 10000.00
//      10:00:10.000 4012888888881881   150.00
//      10:00:11.000 4012888888881881   150.00
//      10:00:12.000 4012888888881881   150.00
//      10:00:14.000 4012888888881881   150.00
//      10:05:55.000 4111111111111111   100.00
//      11:00:14.000 4012888888881881   150.00


      val now = new Date(2014,10,10,10,0,1)

      FraudDetectorService.process(Transaction("4111111111111111", new Date(2014,10,10,10,0,1), BigDecimal(150)))
      FraudDetectorService.process(Transaction("4111111111111111", new Date(2014,10,10,10,0,5), BigDecimal(10000)))
      FraudDetectorService.process(Transaction("4012888888881881", new Date(2014,10,10,10,0,10), BigDecimal(150)))
      FraudDetectorService.process(Transaction("4012888888881881", new Date(2014,10,10,10,0,11), BigDecimal(150)))
      FraudDetectorService.process(Transaction("4012888888881881", new Date(2014,10,10,10,0,12), BigDecimal(150)))
      FraudDetectorService.process(Transaction("4012888888881881", new Date(2014,10,10,10,0,14), BigDecimal(150)))
      FraudDetectorService.process(Transaction("4111111111111111", new Date(2014,10,10,10,5,55), BigDecimal(100)))
      FraudDetectorService.process(Transaction("4012888888881881", new Date(2014,10,10,11,0,14), BigDecimal(150)))
    }
  }

}
