package alvaro.fpInScala

import org.scalatest.{Matchers, WordSpecLike}

//Write a generic function map2, that combines two Option values using a binary function.
//If either Option value is None, then the return value is too.
class Exercise4_3Spec extends WordSpecLike with Matchers {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  "map2" should {
    "return Some(3) for Some(1) and Some(2)" in {
      map2(Some(1), Some(2))((a, b) => a + b) shouldBe Some(3)
    }

    "return None for None and Some(2)" in {
      map2(None, Some(2))((a, b) => b) shouldBe None
    }

    "return None for Some(1) and None" in {
      map2(Some(1), None)((a, b) => a) shouldBe None
    }
  }
}
