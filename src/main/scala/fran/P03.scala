package fran

import java.util.NoSuchElementException

import scala.annotation.tailrec

object P03 {

  @tailrec
  def nth[T](n: Int, l: List[T]): T = n match {
    case _ if n>=l.size => throw new NoSuchElementException()
    case 0 => l.head
    case _ => nth(n-1, l.tail)
  }
}
