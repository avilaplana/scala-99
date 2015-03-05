package haroon

import scala.collection.immutable.{Nil, List}

object P05 {

  def reverse(list: List[Any]) : Any = {
    var result: List[Any] = Nil
    var newList = list
    while (!newList.isEmpty) {
      result = newList.head :: result
      newList = newList.tail
    }
    result
  }
}
