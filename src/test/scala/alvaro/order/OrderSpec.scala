package alvaro.order

import org.scalatest.{Matchers, WordSpec}

class OrderSpec extends WordSpec with Matchers{

  case class Test(name: String, id: Int)

  "list of Test objects mercedes, alvaro, rafa" should {
    "be the order alvaro, mercedes rafa when the order is alvaro, mercedes, rafa, maria, pablo, consuelo" in {
      val l = Seq("alvaro", "mercedes", "rafa", "maria", "pablo", "consuelo")
      val l2 = Seq(Test("mercedes", 0), Test("alvaro", 1), Test("rafa", 3))
      l.map(i => l2.find(_.name == i)).flatten shouldBe  Seq(Test("alvaro", 1), Test("mercedes", 0), Test("rafa", 3))
    }
  }
}


