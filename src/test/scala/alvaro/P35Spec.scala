package alvaro

import org.scalatest.{Matchers, WordSpecLike}

/*
Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order.
scala> 315.primeFactors
res0: List[Int] = List(3, 3, 5, 7)
 */
object P35 {

  import P31._

  def primeFactors(n: Int): List[Int] = {
    def nextPrime(p: Int): Int = if (isPrime(p + 1)) p + 1 else nextPrime(p + 1)
    def div(s: Int, p: Int): (Int, Int) = (s % p, s / p)

    def pf(nf: Int, np: Int): List[Int] = {
      isPrime(nf) match {
        case true => List(nf)
        case false =>
          div(nf, np) match {
            case (0, d) => np :: pf(d, 2)
            case (_, d) => pf(nf, nextPrime(np))
          }
      }
    }
    pf(n, 2)
  }
}

class P35Spec extends WordSpecLike with Matchers {

  "primeFactors" should {
    import P35._

    "should return List(3,5) when the number is 15" in {
      primeFactors(15) shouldBe List(3, 5)
    }
    "should return List(2,2,2,11) when the number is 88" in {
      primeFactors(88) shouldBe List(2, 2, 2, 11)
    }
    "should return List(2,5,23) when the number is 230" in {
      primeFactors(230) shouldBe List(2, 5, 23)
    }
    "should return List(2, 23) when the number is 46" in {
      primeFactors(46) shouldBe List(2, 23)
    }
    "should return List(3,3,5,7) when the number is 315" in {
      primeFactors(315) shouldBe List(3, 3, 5, 7)
    }
    "should return List(2,3, 3, 3,7, 11, 109) when the number is 453222" in {
      primeFactors(453222) shouldBe List(2,3, 3, 3,7, 11, 109)
    }
  }
}
