package alvaro.supermarket.exercise1

import org.scalatest.{Matchers, WordSpecLike}

//Step 1: Shopping cart
//·         You are building a checkout system for a shop which only sells apples and oranges.
//·         Apples cost 60p and oranges cost 25p.
//·         Build a checkout system which takes a list of items scanned at the till and outputs the total cost
//·         For example: [ Apple, Apple, Orange, Apple ] => £2.05
//·         Make reasonable assumptions about the inputs to your solution; for example, many candidates take a list
// of strings as input

sealed trait Fruit {
  val price: Int
}

case class Apple(price: Int) extends Fruit

case class Orange(price: Int) extends Fruit

object Supermarket {
  def checkout(fruits: Seq[Fruit]): Int = fruits.map(_.price).sum
}

class Supermarket1Spec extends WordSpecLike with Matchers {

  "Buy an apple" should {
    "cost 60" in {
      Supermarket.checkout(Seq(Apple(60))) shouldBe 60
    }
  }


  "Buy an orange" should {
    "cost 25" in {
      Supermarket.checkout(Seq(Orange(25))) shouldBe 25
    }
  }

  "Buy 2 apples" should {
    "cost 120" in {
      Supermarket.checkout(Seq(Apple(60), Apple(60))) shouldBe 120
    }
  }

  "Buy 2 oranges" should {
    "cost 50" in {
      Supermarket.checkout(Seq(Orange(25), Orange(25))) shouldBe 50
    }
  }

  "Buy 2 oranges and apple" should {
    "cost 110" in {
      Supermarket.checkout(Seq(Orange(25), Orange(25), Apple(60))) shouldBe 110
    }
  }
}
