package alvaro.fizzBuzz.step1

import org.scalatest.{Matchers, WordSpecLike}

import scala.collection.immutable.Iterable


//Enhance your existing solution to perform the following:
//
//Produce a report at the end of the programme showing how many times the following were output
//fizz
//buzz
//fizzbuzz
//lucky
//an integer
//e.g. if I run the program over a range from 1-20 I should get the following output
//
//1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz
//fizz: 4
//buzz: 3
//fizzbuzz: 1
//lucky: 2
//integer: 10
//(Integer is 10 because there were 10 numbers that were not altered in any way).

object FizzBuzz3 {
  def play(numbers: Seq[Int]): String = {
    def p(number: Int) =
      number match {
        case n if n.toString.contains("3") => "lucky"
        case n if (n % 15 == 0) => "fizzbuzz"
        case n if (n % 5 == 0) => "buzz"
        case n if (n % 3 == 0) => "fizz"
        case n => n.toString
      }

    def isInt(k: String) = !Seq("lucky", "fizzbuzz", "buzz", "fizz").contains(k)

    def report(r: Map[String, Int]) = {
      Seq("fizz", "buzz", "fizzbuzz", "lucky", "integer").map {
        e => s"$e: ${r.get(e).getOrElse("0")}"
      }.mkString("\n")
    }

    val n = numbers.map(p(_))
    val gameSolution = n.mkString(" ")
    val repetitions = n.map {
      case e if (isInt(e)) => "integer"
      case e => e
    }.groupBy(p => p).map(e => (e._1, e._2.size))

    s"""$gameSolution
        |${report(repetitions)}""".stripMargin
  }

}

class Step3Spec extends WordSpecLike with Matchers {

  "1" should {
    "print 1 and integer:1" in {
      val output = """1
                     |fizz: 0
                     |buzz: 0
                     |fizzbuzz: 0
                     |lucky: 0
                     |integer: 1""".stripMargin
      FizzBuzz3.play(Seq(1)) shouldBe output
    }
  }

  "1,2" should {
    "print 1 2 and integer:2" in {
      val output = """1 2
                     |fizz: 0
                     |buzz: 0
                     |fizzbuzz: 0
                     |lucky: 0
                     |integer: 2""".stripMargin
      FizzBuzz3.play(Seq(1, 2)) shouldBe output
    }
  }

  "1,2,3" should {
    "print 1 2 lucky and lucky: 1 integer:2" in {
      val output = """1 2 lucky
                     |fizz: 0
                     |buzz: 0
                     |fizzbuzz: 0
                     |lucky: 1
                     |integer: 2""".stripMargin
      FizzBuzz3.play(Seq(1, 2, 3)) shouldBe output
    }
  }

  "1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz" should {
    "print the new format" in {
      val output = """1 2 lucky 4 buzz fizz 7 8 fizz buzz 11 fizz lucky 14 fizzbuzz 16 17 fizz 19 buzz
                     |fizz: 4
                     |buzz: 3
                     |fizzbuzz: 1
                     |lucky: 2
                     |integer: 10""".stripMargin

      FizzBuzz3.play(Range(1, 21)) shouldBe output

    }
  }
}
