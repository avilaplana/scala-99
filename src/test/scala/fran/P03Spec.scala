package fran

import java.util.NoSuchElementException

import fran.P03.nth
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class P03Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "P03"

  it should "throw exception for the kth element of a list of n elements when k>=n" in {
    val points = for {
      size <- Gen.chooseNum(1, 10)
      list <- Gen.listOfN(size, Gen.choose(1,5))
      shift <- Gen.chooseNum(0,5)
    } yield (size, list, shift)

    forAll(points) { case (size, list, shift) =>
      intercept[NoSuchElementException] {
        nth(size+shift, list)
      }
    }
  }

  it should "return the kth element of a list of n elements when k<n" in {
    val points = for {
      size <- Gen.chooseNum(1, 10)
      list <- Gen.listOfN(size, Gen.choose(1,5))
      k <- Gen.chooseNum(0, size-1)
    } yield (size, list, k)

    forAll(points) { case (size, list, k) =>
      nth(k, list) shouldBe list(k)
    }
  }

  it should "pass the given example" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
  }

}
