package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P07 {
  def flatten(list: List[Any]): List[Int] = {
    def f(lt: List[Int], l: List[Any]): List[Int] =
      l match {
        case Nil => lt
        case (head: Int) :: _ => f(lt :+ head, l.tail)
        case (head: List[Any]) :: _  => f(f(lt, head), l.tail)
        case _ => throw new IllegalArgumentException("Error type of the arguments")
      }
    f(Nil, list)
  }
}

class P07Spec extends WordSpecLike with Matchers {

  "flatten" should {
    import P07._
    "return empty list when the list is empty" in {
      flatten(Nil) shouldBe Nil
    }
    "return list (1) when the list is List(1)" in {
      flatten(List(1)) shouldBe List(1)
    }
    "return list (1, 2) when the list is List(1), List(2)" in {
      flatten(List(1, 2)) shouldBe List(1, 2)
    }
    "return list (1,2) when the list is 1, List(2)" in {
      flatten(List(1, List(2))) shouldBe List(1, 2)
    }
    "return list (1,2,3) when the list is 1, List(2, List(3))" in {
      flatten(List(1, List(2, List(3)))) shouldBe List(1, 2, 3)
    }
    "return list (1, 1, 2, 3, 5, 8) when the list is List(1, 1), 2, List(3, List(5, 8)))" in {
      flatten(List(List(1, 1), 2, List(3, List(5,8)))) shouldBe List(1, 1, 2, 3, 5, 8)
    }
  }
}
