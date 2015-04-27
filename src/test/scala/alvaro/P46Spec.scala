package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P46 {

  def and(a: Boolean, b: Boolean) = a && b

  def nand(a: Boolean, b: Boolean) = !and(a, b)

  def or(a: Boolean, b: Boolean) = a || b

  def xor(a: Boolean, b: Boolean) = and(!and(a, b), or(a, b))

  def nor(a: Boolean, b: Boolean) = !or(a, b)

  def equ(a: Boolean, b: Boolean) = and(and(a, b), !or(a, b))
}

class P46Spec extends WordSpecLike with Matchers {

  val table2: (Boolean, Boolean) => Boolean = {
    import P46._
    (a, b) => and(a, or(a, b))
  }

  "and" should {
    "be true when a is true and b is true" in {
      table2(true, true) shouldBe true
    }

    "be true when a is true and b is false" in {
      table2(true, false) shouldBe true
    }

    "be false when a is false and b is true" in {
      table2(false, true) shouldBe false
    }

    "be false when a is false and b is false" in {
      table2(false, false) shouldBe false
    }
  }
}
