package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Extract a given number of randomly selected elements from a list.
//scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//res0: List[Symbol] = List('e, 'd, 'a)
class P23Spec extends WordSpecLike with Matchers {

  val randomSelect: (List[Int], List[Char]) => List[Char] = (ex, l) => l.zipWithIndex.filter(c => ex.contains(c._2 + 1)).map(_._1)


  "randomSelect" should {
    "return List('a') when the list is  List('a') and the elements to extract is 1" in {
      randomSelect(List(1), List('a')) shouldBe List('a')
    }

    "return List('a') when the list is  List('a', 'b') and the elements to extract are 1" in {
      randomSelect(List(1), List('a','b')) shouldBe List('a')
    }

    "return List('b') when the list is  List('a', 'b') and the elements to extract are 2" in {
      randomSelect(List(2), List('a','b')) shouldBe List('b')
    }

    "return List('b','c') when the list is  List('a', 'b','c') and the elements to extract are 2 and 3" in {
      randomSelect(List(2, 3), List('a', 'b', 'c')) shouldBe List('b', 'c')
    }

    "return List('b','c') when the list is  List('a', 'b','c') and the elements to extract are 3, 4 and 6" in {
      randomSelect(List(3, 4, 6), List('a', 'b', 'c', 'd', 'e', 'f')) shouldBe List('c', 'd', 'f')
    }
  }
}
