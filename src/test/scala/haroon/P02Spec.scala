package haroon

import org.scalatest.{Matchers, WordSpec}
import scala.annotation.tailrec

object P02 {

  @tailrec
  def penultimate(list: List[Any]) : Any = {
    list match {
      case Nil => None
      case l if(l.size == 1) => None
      case head :: tail if tail.length == 1 => head
      case head :: tail => penultimate(tail)
    }
  }
}
class P02Spec extends WordSpec with Matchers {

  "P02 Spec" should {

    "return Nil on empty list" in {
      P02.penultimate(List()) shouldBe None
    }

    "return nil on nil list" in {
      P02.penultimate(Nil) shouldBe None
    }

    "return None for a list with one element" in {
      P02.penultimate(List(2)) shouldBe None

    }

    "return last element for a list with more than one elements of same type" in {
      assert(P02.penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
    }

    "return last element for a list with more than one elements of different types" in {
      assert(P02.penultimate(List("bar",3,4,"foo",5)) == "foo")
    }
  }
}
