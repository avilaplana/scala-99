package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P36 {

  import P31._

  implicit class IntPrimeFactors(n: Int) {
    def primeFactors: List[Int] = {
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

  implicit class ListPrimeFactors(l: List[Int]) {
    def toMultilicityMap: Map[Int, Int] = l.foldLeft(Map.empty[Int, Int]) {
      (m, e) => m + m.get(e).map(v => (e,(v + 1))).getOrElse((e, 1))
    }
  }

}

/*
Determine the prime factors of a given positive integer (2).
Construct a list containing the prime factors and their multiplicity.
scala> 315.primeFactorMultiplicity
res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
Alternately, use a Map for the result.

scala> 315.primeFactorMultiplicity
res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
*/

class P36Spec extends WordSpecLike with Matchers {

  "primeFactors" should {
    import P36._

    "should return List(3,5) when the number is 15" in {
      15.primeFactors.toMultilicityMap shouldBe Map(3 -> 1, 5 -> 1)
    }
    "should return List(2,2,2,11) when the number is 88" in {
      88.primeFactors.toMultilicityMap shouldBe Map(2 -> 3, 11 -> 1)
    }
    "should return List(2,5,23) when the number is 230" in {
      230.primeFactors.toMultilicityMap shouldBe Map(2 -> 1, 5 -> 1, 23 -> 1)
    }
    "should return List(2, 23) when the number is 46" in {
      46.primeFactors.toMultilicityMap shouldBe Map(2 -> 1, 23 -> 1)
    }
    "should return List(3,3,5,7) when the number is 315" in {
      315.primeFactors.toMultilicityMap shouldBe Map(3 -> 2, 5 -> 1, 7 -> 1)
    }
    "should return List(2,3, 3, 3,7, 11, 109) when the number is 453222" in {
      453222.primeFactors.toMultilicityMap shouldBe Map(2 -> 1, 3 -> 3, 7 -> 1, 11 -> 1, 109 -> 1)
    }
  }
}
