package fran

object P07 {

  def flatten(l: List[Any]): List[Any] = l flatMap {
    case l:List[Any] => flatten(l)
    case e => List(e)
  }
}