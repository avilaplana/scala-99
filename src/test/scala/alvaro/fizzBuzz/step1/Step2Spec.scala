package alvaro.fizzBuzz.step1

import org.scalatest.{Matchers, WordSpecLike}

//Enhance your existing FizzBuzz solution to perform the following:
//
//If the number contains a three you must output the text 'lucky'. This overrides any existing behaviour
//e.g. if I run the program over a range from 1-20 I should get the following output
//
//1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz
//Archive this as a separate zip file then continue on to step three

object FizzBuzz2 {
  def play(numbers: Seq[Int]): String = {
    def p(n: Int) =
    if (n.toString.contains("3")) "lucky"
    else if (n % 15 == 0) "fizzbuzz"
    else if (n % 5 == 0) "buzz"
    else if (n % 3 == 0) "fizz"
    else n.toString

    numbers.map(p(_)).mkString(" ")
  }
}

class Step2Spec extends WordSpecLike with Matchers {

  Seq(3, 13, 23, 32, 33, 34, 43).foreach {
    e => s"contains 3: $e" should {
      "print lucky" in {
        FizzBuzz2.play(Seq(e)) shouldBe "lucky"
      }
    }
  }


  "1 - 20" should {
    "print 1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz" in {
      FizzBuzz2.play(Range(1, 21)) shouldBe "1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz"
    }
  }
}
