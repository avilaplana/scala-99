package haroon

import scala.annotation.tailrec

object P01 {

  @tailrec
  def last(list: List[Any]) : Any = {
    list match {
      case Nil => None
      case head :: tail if tail == Nil => head
      case head :: tail => last(tail)
    }
  }
}
