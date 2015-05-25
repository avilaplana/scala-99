package alvaro.fpInScala

import org.scalatest.{Matchers, WordSpecLike}

class Exercise4_8Spec extends WordSpecLike with Matchers {
  def sequence[E,A](a: List[Either[E,A]])(default: E): Either[E, List[A]] = {
    a.collect{case Right(b) => b} match {
      case l if l.size == a.size => Right(l)
      case a => Left(default)
    }
  }

  def traverse[E,A,B](a: List[A])(f: A => Either[E,A])(default: E): Either[E, List[A]] = {
    a.collect{e => f(e) match {case Right(c) => c}} match {
      case l if l.size == a.size => Right(l)
      case _ => Left(default)
    }
  }


  "sequence" should {
    "return Some(List(1,2,3,4,5)) for List(Some(1), Some(2), Some(3),Some(4),Some(5))" in {
      sequence[String, Int](List(Right(1), Right(2), Right(3), Right(4), Right(5)))("error") shouldBe Right(List(1, 2, 3, 4, 5))
    }

    "return None for List(Some(1), None, Some(3),Some(4),Some(5))" in {
      sequence(List(Right(1), Left("some value"), Right(1), Right(1), Right(1)))("error") shouldBe Left("error")
    }
  }

  "traverse" should {
    "return Right(List(1,2,3)) for List(1,2,3) and f: x < 5 Right(x) otherwise Left(\"error\")" in {
      traverse(List(1,2,3)){e => if (e < 5) Right(e) else Left("error")}("error") shouldBe Right(List(1,2,3))
    }

    "return Left(\"error\") for List(1,2,3) and f: x < 2 Right(x) otherwise Left(\"error\")" in {
      traverse(List(1,2,3)){e => if (e < 2) Right(e) else Left("e")}("error") shouldBe Left("error")
    }
  }
}
