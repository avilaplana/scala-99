package alvaro

import org.scalatest.{Matchers, WordSpecLike}

object P47 {

  implicit class BooleanTable(a: Boolean)  {
    def and(b: Boolean) = a && b

    def nand(b: Boolean) = !(a and b)

    def or(b: Boolean) = a || b

    def xor(b: Boolean) = !(a and b) and (a or b)

    def nor(b: Boolean) = !(a or b)

    def equ(b: Boolean) = (a and b) and !(a or b)
  }
}

class P47Spec extends WordSpecLike with Matchers {

  val table2: (Boolean, Boolean) => Boolean = {
    import P47._
    (a, b) => a and (a or !b)
  }

  "and" should {
    "be true when a is true and b is true" in {
      table2(true, true) shouldBe true
    }

    "be false when a is true and b is false" in {
      table2(true, false) shouldBe true
    }

    "be true when a is false and b is true" in {
      table2(false, true) shouldBe false
    }

    "be false when a is false and b is false" in {
      table2(false, false) shouldBe false
    }
  }
}
