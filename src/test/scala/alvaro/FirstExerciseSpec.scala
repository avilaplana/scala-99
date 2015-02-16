package alvaro

import org.scalatest.{Matchers, FlatSpec}

object Finder {
  def last(list: List[Int]): Int = ???
}

class FirstExerciseSpec extends FlatSpec with Matchers {

  import Finder._

  "last" should "return the last element in the list" in {
    val list = List(1, 1, 2, 3, 5, 8)
    last(list) shouldBe 8
  }
}
