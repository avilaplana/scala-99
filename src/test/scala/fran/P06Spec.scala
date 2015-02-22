package fran

import fran.P06.isPalindrome
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class P06Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  behavior of "P05"

  it should "return true for a empty list" in {
    isPalindrome(List()) shouldBe true
  }

  it should "return true for a one-element list" in {
    isPalindrome(List(1)) shouldBe true
  }

  it should "return false for a non palindrome" in {
    isPalindrome(List(1,2,3)) shouldBe false
  }

  it should "return true for any palindrome" in {
    val points = for {
      size <- Gen.chooseNum(0, 10)
      list <- Gen.listOfN(size, Gen.choose(1,10))
      palindrome <- list ::: list.reverse
    } yield (palindrome)

    forAll(points) { case (palindrome) =>
      isPalindrome(palindrome) shouldBe true
    }
  }

  it should "pass the given example" in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }
}
