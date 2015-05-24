package alvaro.fpInScala

import org.scalatest.{Matchers, WordSpecLike}

//EXERCISE 5: Write a function sequence, that combines a list of Options into one option containing a list of all the Some values in the original
// list.
//If the original list contains None even once, the result of the function should be None, otherwise the result should be Some with a list of all
//the values. Here is its signature:5

//This is a clear instance where it's not possible to define the function in the OO style. This should not be a method on List
// (which shouldn't need to know anything about Option), and it can't be a method on Option.
class Exercise4_5Spec extends WordSpecLike with Matchers {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.collect{ case Some(e) => e } match {
      case l if l.size == a.size => Some(l)
      case _ => None
    }
  }

  "sequence" should {
    "return Some(List(1,2,3,4,5)) for List(Some(1), Some(2), Some(3),Some(4),Some(5))" in {
      sequence(List(Some(1), Some(2), Some(3), Some(4), Some(5))) shouldBe Some(List(1, 2, 3, 4, 5))
    }

    "return None for List(Some(1), None, Some(3),Some(4),Some(5))" in {
      sequence(List(Some(1), None, Some(3), Some(4), Some(5))) shouldBe None
    }
  }
}
