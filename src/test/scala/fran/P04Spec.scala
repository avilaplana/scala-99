package fran

import org.scalatest.{FlatSpec, Matchers}

class P04Spec extends FlatSpec with Matchers {

  behavior of "P04"

  it should "return 0 for a empty list" in {
    P04.length(List()) shouldBe 0
  }

  it should "return 1 for a one-element list" in {
    P04.length[Int](List(4)) shouldBe 1
  }

  it should "pass the given example" in {
    P04.length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

}
