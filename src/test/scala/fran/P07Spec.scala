package fran

import fran.P07.flatten
import org.scalatest.{FlatSpec, Matchers}

class P07Spec extends FlatSpec with Matchers {

  behavior of "P07"

  it should "return the same list for a non-recursive list" in {
    flatten(List(1,2,3)) shouldBe List(1,2,3)
  }

  it should "flatten any given list" in {
    flatten(List(List(1,2,3),4)) shouldBe List(1,2,3,4)
  }

  it should "flatten any given list of mixed elements" in {
    flatten(List(List(1, 1), "x", List(3, List(5, "y")))) shouldBe List(1, 1, "x", 3, 5, "y")
  }

  it should "pass the given example" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }
}
