package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

object P08 {
  def compress(list: List[Char]): List[Char] = {
    @tailrec
    def c(lt: List[Char], l: List[Char]): List[Char] =
      (lt, l) match {
        case (_, Nil) => lt
        case (Nil, head :: _) => c(lt :+ head, l.tail)
        case (_, head :: _) if (head == lt.last) => c(lt, l.tail)
        case _ => c(lt :+ l.head, l.tail)
      }
    c(Nil, list)
  }
}

class P08Spec extends WordSpecLike with Matchers {

  "compress" should {
    import P08._
    "return empty List when the list is empty" in {
      compress(Nil) shouldBe Nil
    }
    "return list (a) when the list is (a)" in {
      compress(List('a')) shouldBe List('a')
    }
    "return list (a,b) when the list is (a,b)" in {
      compress(List('a', 'b')) shouldBe List('a', 'b')
    }
    "return list (a,b) when the list is (a,a,b)" in {
      compress(List('a', 'a', 'b')) shouldBe List('a', 'b')
    }
    "return list (a,b) when the list is (a,b,b)" in {
      compress(List('a', 'b', 'b')) shouldBe List('a', 'b')
    }
    "return list ('a, 'b, 'c, 'a, 'd, 'e') when the list is ('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" in {
      compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
      shouldBe List('a', 'b', 'c', 'a', 'd', 'e')
    }

  }
}
