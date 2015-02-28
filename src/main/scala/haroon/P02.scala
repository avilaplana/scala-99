package haroon

import scala.annotation.tailrec

object P02 {

  @tailrec
  def penultimate(list: List[Any]) : Any = {
    list match {
      case Nil => None
      case l if(l.size == 1) => None
      case head :: tail if tail.length == 1 => head
      case head :: tail => penultimate(tail)
    }
  }
}