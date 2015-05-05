package haroon

import org.scalatest.{WordSpec, Matchers}
import scala.annotation.tailrec

object P01 {

  @tailrec
  def last(list: List[Any]) : Any = {
    list match {
      case Nil => None
      case head :: tail if tail == Nil => head
      case head :: tail => last(tail)
    }
  }
}
class P01Spec extends WordSpec with Matchers {

  "P01 Spec" should {

    "return Nil on empty list" in {
      P01.last(List()) shouldBe None
    }

    "return nil on nil list" in {
      P01.last(Nil) shouldBe None
    }

    "return last element for a list with one element" in {
      assert(P01.last(List(2)) == 2)

    }

    "return last element for a list with more than one elements of same type" in {
      assert(P01.last(List(1, 1, 2, 3, 5, 8)) == 8)
    }

    "return last element for a list with more than one elements of different types" in {
      assert(P01.last(List("bar",3,4,5,"foo")) == "foo")
    }
  }
}
