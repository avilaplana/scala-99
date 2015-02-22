package fran

object P05 {

  def reverse[T](l: List[T]): List[T] = {
    l.foldLeft(List[T]()){ (tmpList,el) => el::tmpList }
  }
}
