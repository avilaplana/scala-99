package alvaro.fpInScala.exercise4_7

import org.scalatest.{Matchers, WordSpecLike}


trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case _ => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a,b))
      case (Left(e), _) => Left(e)
    }
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


class Exercise4_7Spec extends WordSpecLike with Matchers {

  "map" should {
    "return Right(3) for Right(2)" in {
      Right(2).map(_+1) shouldBe Right(3)
    }

    "return Left(\"error\") for Left(\"error\")" in {
      Left("error").map("error") shouldBe Left("error")
    }
  }

  "flatMap" should {
    "return Right(3) for f:2 => Right(3)" in {
      Right(2).flatMap(e => Right(e+1)) shouldBe Right(3)
    }

    "return Left(\"error in the function\") for f:2 => Left(\"error in the function\")" in {
      Right(2).flatMap(e => Left("error in the function")) shouldBe Left("error in the function")
    }

    "return Left(\"error\") for Left(\"error\")" in {
      Left("error").flatMap(_ => Right(2)) shouldBe Left("error")
    }
  }

  "orElse" should {
    "return from Right(2) to Right(2)" in {
      Right(2).orElse(Right(5)) shouldBe Right(2)
    }

    "return from Left(\"error\") to Right(5)" in {
      Left("error").orElse(Right(5)) shouldBe Right(5)
    }
  }

  "map2" should {
    "return Right(5) for Right(3), Right(2) f: a + b" in {
      Right(3).map2(Right(2))((a,b) => a + b) shouldBe Right(5)
    }

    "return Left(\"error\") for Left(\"error\"), Right(2) f: a + b" in {
      Left("error").map2(Right(2))((a,b) => b) shouldBe Left("error")
    }
  }
}
