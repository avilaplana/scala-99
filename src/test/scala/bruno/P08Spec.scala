package bruno

import org.scalatest.{WordSpecLike, Matchers}

object P08 {
  def compress[A](lst: List[A]): List[A] = lst.foldRight(List.empty[A]) {
    case (a, b) if b.headOption.contains(a) => b
    case (a, b) => a :: b
  }
}

class P08Spec extends WordSpecLike with Matchers {

  "compress" should {
    "eliminate consecutive duplicates of list elements" in {
      P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should contain theSameElementsInOrderAs List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }
}
