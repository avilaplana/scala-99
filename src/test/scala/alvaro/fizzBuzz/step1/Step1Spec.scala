package alvaro.fizzBuzz.step1

import org.scalatest.{Matchers, WordSpecLike}

//Step 1:
//
//Write some code that prints out the following for a contiguous range of numbers:
//
//the number
//'fizz' for numbers that are multiples of 3
//'buzz' for numbers that are multiples of 5
//'fizzbuzz' for numbers that are multiples of 15
//e.g. if I run the program over a range from 1-20 I should get the following output
//
//1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz
//Archive this as a separate zip file then continue on to step two.
object FizzBuzz {
  def play(numbers: Seq[Int]): String = {
    def p(n: Int) =
      if (n % 15 == 0) "fizzbuzz"
      else if (n % 5 == 0) "buzz"
      else if (n % 3 == 0) "fizz"
      else n.toString
    numbers.map(p(_)).mkString(" ")
  }
}

class Step1Spec extends WordSpecLike with Matchers {

  Seq(1, 2, 4, 7, 8, 11).foreach {
    e => s"no multiples of 3,5,15: $e" should {
      s"print $e" in {
        FizzBuzz.play(Seq(e)) shouldBe e.toString
      }
    }
  }

  Seq(3, 6, 9, 12, 18, 21).foreach {
    e => s"multiples of 3: $e" should {
      "print fizz" in {
        FizzBuzz.play(Seq(e)) shouldBe "fizz"
      }
    }
  }

  Seq(5, 10, 20, 25).foreach {
    e => s"multiples of 5: $e" should {
      "print buzz" in {
        FizzBuzz.play(Seq(e)) shouldBe "buzz"
      }
    }
  }

  Seq(15, 30, 45).foreach {
    e => s"multiples of 15: $e" should {
      "print fizzbuzz" in {
        FizzBuzz.play(Seq(e)) shouldBe "fizzbuzz"
      }
    }
  }

  "1 - 20" should {
    "print 1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz" in {
      FizzBuzz.play(Range(1, 21)) shouldBe
        "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz 16 17 fizz 19 buzz"
    }
  }

}
