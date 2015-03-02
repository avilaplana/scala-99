package haroon

import haroon.P01._
import org.scalatest.{WordSpec, Matchers, FlatSpec}

class P01Spec extends WordSpec with Matchers {

  "P01 Spec" should {

    "return Nil on empty list" in {
      last(List()) shouldBe None
    }

    "return nil on nil list" in {
      last(Nil) shouldBe None
    }

    "return last element for a list with one element" in {
      assert(last(List(2)) == 2)

    }

    "return last element for a list with more than one elements of same type" in {
      assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
    }

    "return last element for a list with more than one elements of different types" in {
      assert(last(List("bar",3,4,5,"foo")) == "foo")
    }
  }
}
