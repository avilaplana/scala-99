package haroon

object P04 {

  def length (list: List[Any]) : Int = {
    var len = 0
    var newList = list
    while (!newList.isEmpty) {
      len += 1
      newList = newList.tail
    }
    len
  }
}
