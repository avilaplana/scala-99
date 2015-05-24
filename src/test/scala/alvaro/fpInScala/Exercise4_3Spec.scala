package alvaro.fpInScala

import alvaro.fpInScala.exercise4_1
import org.scalatest.{Matchers, WordSpecLike}

//Write a generic function map2, that combines two Option values using a binary function.
//If either Option value is None, then the return value is too.
class Exercise4_3Spec extends WordSpecLike with Matchers {

  def map2[A, B, C](a: exercise4_1.Option[A], b: exercise4_1.Option[B])(f: (A, B) => C): exercise4_1.Option[C] =
    for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  "map2" should {
    "return Some(3) for Some(1) and Some(2)" in {
      map2(exercise4_1.Some(1), exercise4_1.Some(2))((a, b) => a + b) shouldBe exercise4_1.Some(3)
    }

    "return None for None and Some(2)" in {
      map2(exercise4_1.None, exercise4_1.Some(2))((a, b) => b) shouldBe exercise4_1.None
    }

    "return None for Some(1) and None" in {
      map2(exercise4_1.Some(1), exercise4_1.None)((a, b) => a) shouldBe exercise4_1.None
    }
  }
}
