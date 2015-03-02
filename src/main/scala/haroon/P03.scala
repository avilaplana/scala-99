package haroon

object P03 {

  def nth(index: Int, list: List[Any]) : Any = {
    list.lift(index).getOrElse(None)
  }
}
