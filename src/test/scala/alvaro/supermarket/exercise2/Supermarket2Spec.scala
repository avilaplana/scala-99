package alvaro.supermarket.exercise2

import org.scalatest.{Matchers, WordSpecLike}

//The shop decides to introduce two new offers
//o    buy one, get one free on Apples
//o    3 for the price of 2 on Oranges

sealed trait Fruit {
  val price: Int
}

case class Apple(price: Int) extends Fruit

case class Orange(price: Int) extends Fruit

object Supermarket {
  def checkout(fruits: Seq[Fruit]): Int = {

    def pay(number: Int): Seq[Fruit] => Int = {
      f => f.take(number).map(_.price).sum
    }

    def payApples(apples: Seq[Fruit]) = pay(apples.size / 2 + apples.size % 2)(apples)
    def payOranges(oranges: Seq[Fruit]) = pay((oranges.size / 3) * 2 + oranges.size % 3)(oranges)

    payApples(fruits.filter(_ == Apple(60))) + payOranges(fruits.filter(_ == Orange(25)))
  }
}

class Supermarket2Spec extends WordSpecLike with Matchers {

  "Buy 1 apples" should {
    "cost 60" in {
      Supermarket.checkout(Seq(Apple(60))) shouldBe 60
    }
  }

  "Buy 2 apples" should {
    "cost 60" in {
      Supermarket.checkout(Seq(Apple(60), Apple(60))) shouldBe 60
    }
  }

  "Buy 3 apples" should {
    "cost 120" in {
      Supermarket.checkout(Seq(Apple(60), Apple(60), Apple(60))) shouldBe 120
    }
  }


  "Buy 1 orange" should {
    "cost 25" in {
      Supermarket.checkout(Seq(Orange(25))) shouldBe 25
    }
  }


  "Buy 2 orange" should {
    "cost 50" in {
      Supermarket.checkout(Seq(Orange(25), Orange(25))) shouldBe 50
    }
  }

  "Buy 3 orange" should {
    "cost 50" in {
      Supermarket.checkout(Seq(Orange(25), Orange(25), Orange(25))) shouldBe 50
    }
  }


  "Buy 4 orange" should {
    "cost 75" in {
      Supermarket.checkout(Seq(Orange(25), Orange(25), Orange(25), Orange(25))) shouldBe 75
    }
  }


  "Buy 4 orange and 3 orange" should {
    "cost 125" in {
      Supermarket.checkout(
        Seq(
          Orange(25), Orange(25), Orange(25), Orange(25), Orange(25), Orange(25), Orange(25))
      ) shouldBe 125
    }
  }
}
