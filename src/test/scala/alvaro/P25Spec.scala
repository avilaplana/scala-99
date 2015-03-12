package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//
//Generate a random permutation of the elements of a list.
//Hint: Use the solution of problem P23.
//Example:
//
//scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
class P25Spec extends WordSpecLike with Matchers {

  import P24._

  "randomPermute" should {
    "return a random permutation List('a') when the list is List('a')" in {
      val c = List('a')
      val l = lotto(1, 1)
      l.map(p => c(p - 1)) shouldBe List('a')
    }
    "return a random permutation  when the list is List('a','b')" in {
      val c = List('a', 'b')
      // I could use lotto to create a random permutation of positions
      // lotto(2,2).map(p =>c(p - 1))
      List(2, 1).map(p => c(p - 1)) shouldBe List('b', 'a')
    }

    "return a random permutation  when the list is List('a','b','c')" in {
      val c = List('a', 'b', 'c')
      List(2, 3, 1).map(p => c(p - 1)) shouldBe List('b', 'c', 'a')
    }
    "return a random permutation  when the list is List('a', 'b', 'c', 'd', 'e', 'f')" in {
      val c = List('a', 'b', 'c', 'd', 'e', 'f')
      List(2, 1, 4, 3, 5, 6).map(p => c(p - 1)) shouldBe List('b', 'a', 'd', 'c', 'e', 'f')
    }
  }
}
