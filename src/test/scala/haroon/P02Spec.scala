package haroon

import haroon.P02._
import org.scalatest.{Matchers, WordSpec}

class P02Spec extends WordSpec with Matchers {

  "P02 Spec" should {

    "return Nil on empty list" in {
      penultimate(List()) shouldBe None
    }

    "return nil on nil list" in {
      penultimate(Nil) shouldBe None
    }

    "return None for a list with one element" in {
      penultimate(List(2)) shouldBe None

    }

    "return last element for a list with more than one elements of same type" in {
      assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
    }

    "return last element for a list with more than one elements of different types" in {
      assert(penultimate(List("bar",3,4,"foo",5)) == "foo")
    }
  }
}
