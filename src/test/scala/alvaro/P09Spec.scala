package alvaro

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.collection.:+

object P09 {
  def pack(list: List[Char]): List[List[Char]] = {
    @tailrec
    def p(lt: List[List[Char]], l: List[Char]): List[List[Char]] =
      (lt, l) match {
        case (List(Nil), head :: tail) => p(List(List(head)), tail)
        case (lt, Nil) => lt
        case (init :+ last, head :: tail) if (last.contains(head)) => p(init :+ (last :+ head), tail)
        case _ => p(lt :+ List(l.head), l.tail)
      }
    p(List(Nil), list)
  }
}

class P09Spec extends WordSpecLike with Matchers {

  "pack" should {
    import P09._
    "return empty List(List()) when the list is empty" in {
      pack(Nil) shouldBe List(Nil)
    }
    "return List(List(a)) when the list is List(a)" in {
      pack(List('a')) shouldBe List(List('a'))
    }
    "return List(List(a,a)) when the list is List(a,a)" in {
      pack(List('a', 'a')) shouldBe List(List('a', 'a'))
    }
    "return List(List(a), List(b)) when the list is List(a,b)" in {
      pack(List('a', 'b')) shouldBe List(List('a'), List('b'))
    }
    "return  List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) when " +
      "the list is List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" in {

      val charsToPack = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      pack(charsToPack) shouldBe
        List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))
    }
  }

  "pack with foldLeft" should {
    def pack(l: List[List[Char]], e: Char) =
      l match {
        case List(Nil) => List(List(e))
        case init :+ last if last.contains(e) => init :+ (last :+ e)
        case _ => l :+ List(e)
      }

    "return empty List(List()) when the list is empty" in {
      Nil.foldLeft(List[List[Char]](Nil))(pack _) shouldBe List(Nil)
    }
    "return List(List(a)) when the list is List(a)" in {
      List('a').foldLeft(List[List[Char]](Nil))(pack _) shouldBe List(List('a'))
    }
    "return List(List(a,a)) when the list is List(a,a)" in {
      List('a', 'a').foldLeft(List[List[Char]](Nil))(pack _) shouldBe List(List('a', 'a'))
    }
    "return List(List(a), List(b)) when the list is List(a,b)" in {
      List('a', 'b').foldLeft(List[List[Char]](Nil))(pack _) shouldBe List(List('a'), List('b'))
    }
    "return  List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) when " +
      "the list is List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" in {

      val charsToPack = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      charsToPack.foldLeft(List[List[Char]](Nil))(pack _) shouldBe
        List(
          List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'),
          List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))
    }
  }
}
