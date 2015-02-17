package fran

import java.util.NoSuchElementException

import scala.annotation.tailrec

object P01 {

  @tailrec
  def last[T](l: List[T]): T = {
    if (l.size==0) throw new NoSuchElementException
    else if (l.size==1) l.head
    else last(l.tail)
  }
}
