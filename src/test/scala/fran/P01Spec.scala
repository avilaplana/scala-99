package fran

import java.util.NoSuchElementException

import fran.P01.last
import org.scalatest.{FlatSpec, Matchers}

class P01Spec extends FlatSpec with Matchers {

  behavior of "P01"

  it should "return None if an empty list is passed on" in {
    intercept[NoSuchElementException] {
      last(List())
    }
  }

  it should "return the only element in a one-element list" in {
    last(List(1)) shouldBe 1
  }

  it should "return the second element in a two-element list" in {
    last(List("a", "b")) shouldBe "b"
  }

  it should "return the last element in a 8 element list" in {
    last(List(1,2,3,4,5,6,7,8)) shouldBe 8
  }

  it should "pass the given example" in {
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
  }

}
