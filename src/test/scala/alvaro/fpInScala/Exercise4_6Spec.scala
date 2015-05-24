package alvaro.fpInScala

import org.scalatest.{Matchers, WordSpecLike}

//EXERCISE 6: Implement this function. It is straightforward to do using map and sequence, but try for a more efficient implementation that only
//looks at the list once. In fact, implement sequence in terms of traverse.
class Exercise4_6Spec extends WordSpecLike with Matchers {

        def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
          a.collect{e => f(e) match {case Some(c) => c}} match {
            case l if l.size == a.size => Some(l)
            case _ => None
          }
        }

  "traverse" should {
    "return Some(List(1,2,3)) for List(1,2,3) and f: x < 5 Some(x) otherwise None" in {
      traverse(List(1,2,3)){e => if (e < 5) Some(e) else None} shouldBe Some(List(1,2,3))
    }

    "return None for List(1,2,3) and f: x < 2 Some(x) otherwise None" in {
      traverse(List(1,2,3)){e => if (e < 2) Some(e) else None} shouldBe None
    }
  }

}
