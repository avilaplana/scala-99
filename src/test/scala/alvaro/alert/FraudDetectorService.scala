package alvaro.alert

import java.util.Date


case class Transaction(creditCardNumber: String, timestamp: Date, amount: BigDecimal)

case class Alert(transaction: Transaction, message: String)

trait FraudDetectorService {

  val alertService: FraudAlertService

  val transactionsRepo: scala.collection.mutable.Map[String, Seq[Transaction]] = scala.collection.mutable.Map.empty

  def process(transaction: Transaction) = {

    def moreThan10000(sortedTrans: Seq[Transaction]) = {

      val sl = sortedTrans.takeWhile(t => t.timestamp.getTime + 30 * 60 * 1000 > sortedTrans.head.timestamp.getTime)
      sl.map(_.amount).sum > 10000
    }

    def threeLess30Seconds(sortedTrans: Seq[Transaction]) = {
      sortedTrans.takeWhile(t => sortedTrans.head.timestamp.getTime - t.timestamp.getTime < 30000).size == 3
    }

    if (transactionsRepo.keySet.contains(transaction.creditCardNumber)) {
      val currentTrans: Seq[Transaction] = transactionsRepo(transaction.creditCardNumber)
      val updatedTrans: Seq[Transaction] = transaction +: currentTrans
      transactionsRepo += (transaction.creditCardNumber -> updatedTrans)
    } else {
      transactionsRepo += (transaction.creditCardNumber -> Seq(transaction))
    }


    val sorted = transactionsRepo(transaction.creditCardNumber).sortWith((t1, t2) => t1.timestamp.after(t2.timestamp))
    if (moreThan10000(sorted)) alertService.alert(Alert(transaction, "Amount exceeded threshold"))
    if (threeLess30Seconds(sorted)) alertService.alert(Alert(transaction, "Number of transactions exceeded threshold"))

  }
}

object FraudDetectorService extends FraudDetectorService {
  override val alertService: FraudAlertService = FraudAlertService
}

trait FraudAlertService {
  def alert(alert: Alert) = println(s"${alert.transaction.timestamp} - ${alert.transaction.creditCardNumber} - ${alert.message}")
}

object FraudAlertService extends FraudAlertService



