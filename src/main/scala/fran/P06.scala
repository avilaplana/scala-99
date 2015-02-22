package fran

import scala.annotation.tailrec

object P06 {

  @tailrec
  def isPalindrome[T](l: List[T]): Boolean = l match {
    case Nil => true
    case _ if (l.take(1) == l.takeRight(1)) => isPalindrome(l.drop(1).dropRight(1))
    case _ => false
  }
}
