package fran

import scala.annotation.tailrec

object P04 {

  def length[T](l: List[T]): Integer = {
    @tailrec
    def count(n:Int, l:List[T]): Integer = l match {
      case h :: Nil => n+1
      case h :: t => count(n+1, t)
      case _ => 0
    }
    count(0, l)
  }
}
