package fran

import java.util.NoSuchElementException

import scala.annotation.tailrec

object P02 {

  @tailrec
  def penultimate[T](l: List[T]): T = l match {
    case a :: _ :: Nil => a
    case _ :: rest => penultimate(rest)
    case _ => throw new NoSuchElementException
  }
}
