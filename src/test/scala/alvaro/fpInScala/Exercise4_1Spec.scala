package alvaro.fpInScala

import org.scalatest.{Matchers, WordSpecLike}

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(s) => s
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(s) => Some(s)
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(s) if f(s) => Some(s)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

//EXERCISE 1: We'll explore when you'd use each of these next. But first, as an exercise, implement all of the above functions on Option.
// As you implement each function, try to think about what it means and in what situations you'd use it.
// Here are a few hints:
//1. It is fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse without resorting
// to pattern matching.
//2. For map and flatMap, the type signature should be sufficient to determine the implementation.
//3. getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
//4. orElse returns the first Option if it is defined, otherwise, returns the second Option.


class Exercise4_1Spec extends WordSpecLike with Matchers {

  "map" should {
    "return from Some(1) to Some(2)" in {
      Some(1).map(_ + 1) shouldBe Some(2)
    }

    "return None into None" in {
      None.map(n => n) shouldBe None
    }
  }

  "flatMap" should {
    "return from Some(1) to Some(2)" in {
      Some(1).flatMap(s => Some(s + 1)) shouldBe Some(2)
    }

    "return None into None" in {
      None.flatMap(n => n) shouldBe None
    }
  }

  "getOrElse" should {
    "return from Some(1) to 1" in {
      Some(1).getOrElse("I dont care") shouldBe 1
    }

    "return from None to 'I dont care'" in {
      None.getOrElse("I dont care") shouldBe "I dont care"
    }

    "orElse" should {
      "return from Some(1) to Some(1)" in {
        Some(1).orElse(Some("I dont care")) shouldBe Some(1)
      }

      "return from None to 'I dont care'" in {
        None.orElse(Some("I dont care")) shouldBe Some("I dont care")
      }
    }

    "filter" should {
      "return Some(1) when the 1 > 0" in {
        Some(1).filter(_ > 0) shouldBe Some(1)
      }

      "return None becuase 1 is not > 2'" in {
        Some(1).filter(_ > 2) shouldBe None
      }

      "return None becase the filter of None is None" in {
        None.filter(n => n) shouldBe None
      }

    }
  }
}
